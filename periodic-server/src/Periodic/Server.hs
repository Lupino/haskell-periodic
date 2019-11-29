{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Periodic.Server
  ( startServer
  ) where

import           Control.Monad              (forever, mzero, unless, void, when)
import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Maybe  (runMaybeT)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.ByteString            (ByteString)
import           Data.Either                (isLeft)
import           Network.Socket             (Socket, accept)
import qualified Network.Socket             as Socket (close)
import           Periodic.Connection
import qualified Periodic.Connection        as Conn
import           Periodic.IOHashMap         (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap         as HM
import           Periodic.Server.Client
import qualified Periodic.Server.Client     as Client
import           Periodic.Server.Persist    (Persist, PersistConfig)
import           Periodic.Server.Scheduler
import           Periodic.Server.Worker
import qualified Periodic.Server.Worker     as Worker
import           Periodic.Transport         (Transport, TransportConfig)
import           Periodic.Types             (ClientType (..), runParser)
import           Periodic.Utils             (getEpochTime)
import           System.Log.Logger          (errorM, infoM)
import           UnliftIO
import           UnliftIO.Concurrent        (threadDelay)

type ClientList db tp = IOHashMap ByteString (ClientEnv db tp)
type WorkerList db tp = IOHashMap ByteString (WorkerEnv db tp)

data ServerEnv db tp = ServerEnv
  { serveSock  :: Socket
  , serveState :: TVar Bool
  , clientList :: ClientList db tp
  , workerList :: WorkerList db tp
  }


newtype ServerT db tp m a = ServerT {unServerT :: ReaderT (ServerEnv db tp) (SchedT db tp m) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (ServerEnv db tp)
    )

instance MonadTrans (ServerT db tp) where
  lift = ServerT . lift . lift

instance MonadUnliftIO m => MonadUnliftIO (ServerT db tp m) where
  askUnliftIO = ServerT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runServerT r))
  withRunInIO inner = ServerT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runServerT r)

runServerT :: ServerEnv db tp -> ServerT db tp m a -> SchedT db tp m a
runServerT sEnv = flip runReaderT sEnv . unServerT

liftS :: Monad m => SchedT db tp m a -> ServerT db tp m a
liftS = ServerT . lift

initServerEnv :: MonadIO m => TVar Bool -> Socket -> m (ServerEnv db tp)
initServerEnv serveState serveSock = do
  clientList <- newIOHashMap
  workerList <- newIOHashMap
  pure ServerEnv{..}

serveForever :: (MonadUnliftIO m, Persist db, Transport tp) => (Socket -> TransportConfig tp) -> ServerT db tp m ()
serveForever mk = do
  liftIO $ infoM "Periodic.Server" "Server started"
  state <- asks serveState
  void . runMaybeT . forever $ do
    e <- lift $ tryServeOnce mk
    when (isLeft e) mzero
    alive <- readTVarIO state
    unless alive mzero

  liftIO $ infoM "Periodic.Server" "Server closed"

tryServeOnce
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => (Socket -> TransportConfig tp)
  -> ServerT db tp m (Either SomeException ())
tryServeOnce mk = tryAny (serveOnce mk)

serveOnce :: (MonadUnliftIO m, Persist db, Transport tp) => (Socket -> TransportConfig tp) -> ServerT db tp m ()
serveOnce mk = do
  (sock', _) <- liftIO . accept =<< asks serveSock
  void $ async $ handleConnection $ mk sock'

handleConnection
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => TransportConfig tp -> ServerT db tp m ()
handleConnection tpconfig = do
  ServerEnv{..} <- ask
  schedEnv <- liftS ask
  connEnv <- initServerConnEnv tpconfig

  lift $ runConnectionT connEnv $
    receiveThen $ \pl ->
      sendThen $
        case runParser pl of
          Left _           -> Conn.close
          Right TypeClient -> do
            cid <- connid
            liftIO $ infoM "Periodic.Server" ("Client: " ++ show cid ++ " connected")
            clientEnv <- lift $ initClientEnv connEnv schedEnv
            HM.insert clientList cid clientEnv
            lift $ startClientT clientEnv
            liftIO $ infoM "Periodic.Server" ("Client: " ++ show cid ++ " disconnected")
            HM.delete clientList cid
          Right TypeWorker -> do
            cid <- connid
            liftIO $ infoM "Periodic.Server" ("Worker: " ++ show cid ++ " connected")
            workerEnv <- lift $ initWorkerEnv connEnv schedEnv
            HM.insert workerList cid workerEnv
            lift $ startWorkerT workerEnv
            liftIO $ infoM "Periodic.Server" ("Worker: " ++ show cid ++ " disconnected")
            HM.delete workerList cid

  where receiveThen
          :: (MonadUnliftIO m, Transport tp)
          => (ByteString -> ConnectionT tp m ()) -> ConnectionT tp m ()
        receiveThen next = do
          e <- tryAny receive
          case e of
            Left _   -> Conn.close
            Right pl -> next pl

        sendThen
          :: (MonadUnliftIO m, Transport tp)
          => ConnectionT tp m () -> ConnectionT tp m ()
        sendThen next = do
          e <- tryAny $ send =<< connid
          case e of
            Left _  -> Conn.close
            Right _ -> next

runCheckWorkerState
  :: (MonadUnliftIO m, Transport tp)
  => WorkerList db tp -> TVar Int -> m ()
runCheckWorkerState ref alive = runCheckState "Worker" ref (checkWorkerState ref alive) alive

checkWorkerState :: (MonadUnliftIO m, Transport tp) =>  WorkerList db tp -> TVar Int -> WorkerEnv db tp -> m ()
checkWorkerState ref alive env0 = runWorkerT env0 $ do
  delay <- fromIntegral <$> readTVarIO alive
  expiredAt <- (delay +) <$> Worker.getLastVist
  now <- getEpochTime
  when (now > expiredAt) $ do
    Worker.close
    wid <- fromConn connid
    HM.delete ref wid

runCheckClientState
  :: (MonadUnliftIO m, Transport tp)
  => ClientList db tp -> TVar Int -> m ()
runCheckClientState ref alive = runCheckState "Client" ref (checkClientState ref alive) alive

checkClientState :: (MonadIO m, Transport tp) => ClientList db tp -> TVar Int -> ClientEnv db tp -> m ()
checkClientState ref alive env0 = runClientT env0 $ do
  delay <- fromIntegral <$> readTVarIO alive
  expiredAt <- (delay +) <$> Client.getLastVist
  now <- getEpochTime
  when (now > expiredAt) $ do
    Client.close
    cid <- fromConn connid
    HM.delete ref cid

runCheckState
  :: MonadUnliftIO m
  => String -> IOHashMap a b -> (b -> m ()) -> TVar Int -> m ()
runCheckState var ref checkAlive alive = void . async . forever $ do
  delay <- readTVarIO alive
  threadDelay $ fromIntegral delay * 1000 * 1000
  mapM_ checkAlive =<< HM.elems ref
  size <- HM.size ref
  liftIO $ errorM "Periodic.Server" $ "Total " ++ var ++ ": " ++ show size


startServer
  :: (MonadUnliftIO m, Persist db, Transport tp)
  =>  PersistConfig db -> Socket -> (Socket -> TransportConfig tp) -> m ()
startServer config sock mk = do
  state <- newTVarIO True
  sEnv <- initServerEnv state sock
  schedEnv <- initSchedEnv config $ atomically $ writeTVar state False

  runSchedT schedEnv $ do
    startSchedT
    lift . runCheckClientState (clientList sEnv) =<< keepalive
    lift . runCheckWorkerState (workerList sEnv) =<< keepalive

    runServerT sEnv $ serveForever mk

    shutdown
    liftIO $ Socket.close sock
