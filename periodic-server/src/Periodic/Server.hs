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
import           Periodic.Transport         (Transport)
import           Periodic.Types             (ClientType (..), runParser)
import           Periodic.Utils             (getEpochTime)
import           System.Log.Logger          (errorM)
import           UnliftIO
import           UnliftIO.Concurrent        (threadDelay)

type ClientList db = IOHashMap ByteString (ClientEnv db)
type WorkerList db = IOHashMap ByteString (WorkerEnv db)

data ServerEnv db = ServerEnv
  { serveSock  :: Socket
  , serveState :: TVar Bool
  , clientList :: ClientList db
  , workerList :: WorkerList db
  }


newtype ServerT db m a = ServerT {unServerT :: ReaderT (ServerEnv db) (SchedT db m) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (ServerEnv db)
    )

instance MonadTrans (ServerT db) where
  lift = ServerT . lift . lift

instance MonadUnliftIO m => MonadUnliftIO (ServerT db m) where
  askUnliftIO = ServerT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runServerT r))
  withRunInIO inner = ServerT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runServerT r)

runServerT :: ServerEnv db -> ServerT db m a -> SchedT db m a
runServerT sEnv = flip runReaderT sEnv . unServerT

liftS :: Monad m => SchedT db m a -> ServerT db m a
liftS = ServerT . lift

initServerEnv :: MonadIO m => TVar Bool -> Socket -> m (ServerEnv db)
initServerEnv serveState serveSock = do
  clientList <- newIOHashMap
  workerList <- newIOHashMap
  pure ServerEnv{..}

serveForever :: (MonadUnliftIO m, Persist db) => (Socket -> m Transport) -> ServerT db m ()
serveForever mk = do
  state <- asks serveState
  void . runMaybeT . forever $ do
    e <- lift $ tryServeOnce mk
    when (isLeft e) mzero
    alive <- readTVarIO state
    unless alive mzero

tryServeOnce
  :: (MonadUnliftIO m, Persist db)
  => (Socket -> m Transport)
  -> ServerT db m (Either SomeException ())
tryServeOnce mk = tryAny (serveOnce mk)

serveOnce :: (MonadUnliftIO m, Persist db) => (Socket -> m Transport) -> ServerT db m ()
serveOnce makeTransport = do
  (sock', _) <- liftIO . accept =<< asks serveSock
  void $ async $ handleConnection =<< lift (makeTransport sock')

handleConnection
  :: (MonadUnliftIO m, Persist db)
  => Transport -> ServerT db m ()
handleConnection transport = do
  ServerEnv{..} <- ask
  schedEnv <- liftS ask
  connEnv <- initServerConnEnv transport

  lift $ runConnectionT connEnv $
    receiveThen $ \pl ->
      sendThen $
        case runParser pl of
          Left _           -> Conn.close
          Right TypeClient -> do
            cid <- connid
            clientEnv <- lift $ initClientEnv connEnv schedEnv
            HM.insert clientList cid clientEnv
            lift $ startClientT clientEnv
            HM.delete clientList cid
          Right TypeWorker -> do
            cid <- connid
            workerEnv <- lift $ initWorkerEnv connEnv schedEnv
            HM.insert workerList cid workerEnv
            lift $ startWorkerT workerEnv
            HM.delete workerList cid

  where receiveThen
          :: MonadUnliftIO m
          => (ByteString -> ConnectionT m ()) -> ConnectionT m ()
        receiveThen next = do
          e <- tryAny receive
          case e of
            Left _   -> Conn.close
            Right pl -> next pl

        sendThen
          :: MonadUnliftIO m
          => ConnectionT m () -> ConnectionT m ()
        sendThen next = do
          e <- tryAny $ send =<< connid
          case e of
            Left _  -> Conn.close
            Right _ -> next

runCheckWorkerState
  :: MonadUnliftIO m
  => WorkerList db -> TVar Int -> m ()
runCheckWorkerState ref alive = runCheckState "Worker" ref (checkWorkerState ref alive) alive

checkWorkerState :: MonadUnliftIO m =>  WorkerList db -> TVar Int -> WorkerEnv db -> m ()
checkWorkerState ref alive env0 = runWorkerT env0 $ do
  delay <- fromIntegral <$> readTVarIO alive
  expiredAt <- (delay +) <$> Worker.getLastVist
  now <- getEpochTime
  when (now > expiredAt) $ do
    Worker.close
    wid <- fromConn connid
    HM.delete ref wid

runCheckClientState
  :: MonadUnliftIO m
  => ClientList db -> TVar Int -> m ()
runCheckClientState ref alive = runCheckState "Client" ref (checkClientState ref alive) alive

checkClientState :: MonadIO m => ClientList db -> TVar Int -> ClientEnv db -> m ()
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


startServer :: (MonadUnliftIO m, Persist db) => (Socket -> m Transport) -> PersistConfig db -> Socket -> m ()
startServer mk config sock = do
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
