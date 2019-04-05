{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Periodic.Server
  (
    startServer
  ) where

import           Control.Concurrent             (threadDelay)
import           Control.Monad                  (forever, mzero, unless, void,
                                                 when)
import           Control.Monad.Reader.Class     (MonadReader (ask), asks)
import           Control.Monad.Trans.Class      (MonadTrans, lift)
import           Control.Monad.Trans.Maybe      (runMaybeT)
import           Control.Monad.Trans.Reader     (ReaderT (..), runReaderT)
import           Data.ByteString                (ByteString)
import           Data.Either                    (isLeft)
import           Data.String                    (fromString)
import           Network.Socket                 (Socket, accept)
import qualified Network.Socket                 as Socket (close)
import           Periodic.Connection
import qualified Periodic.Connection            as Conn
import           Periodic.IOHashMap             (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap             as HM
import           Periodic.Node                  (liftC)
import           Periodic.Server.Client
import qualified Periodic.Server.Client         as Client
import           Periodic.Server.Persist.SQLite (initSQLite)
import           Periodic.Server.Scheduler
import           Periodic.Server.Worker
import qualified Periodic.Server.Worker         as Worker
import           Periodic.Transport             (Transport)
import           Periodic.Types                 (ClientType (..), runParser)
import           Periodic.Utils                 (getEpochTime)
import           System.Directory               (createDirectoryIfMissing)
import           System.Log.Logger              (errorM)
import           UnliftIO

type ClientList = IOHashMap ByteString ClientEnv
type WorkerList = IOHashMap ByteString WorkerEnv

data ServerEnv = ServerEnv
  { mkTransport :: Socket -> IO Transport
  , serveSock   :: Socket
  , serveState  :: TVar Bool
  , clientList  :: ClientList
  , workerList  :: WorkerList
  }


newtype ServerT m a = ServerT {unServerT :: ReaderT ServerEnv (SchedT m) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader ServerEnv
    )

instance MonadTrans ServerT where
  lift = ServerT . lift . lift

instance MonadUnliftIO m => MonadUnliftIO (ServerT m) where
  askUnliftIO = ServerT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runServerT r))
  withRunInIO inner = ServerT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runServerT r)

runServerT :: ServerEnv -> ServerT m a -> SchedT m a
runServerT sEnv = flip runReaderT sEnv . unServerT

liftS :: Monad m => SchedT m a -> ServerT m a
liftS = ServerT . lift

initServerEnv :: TVar Bool -> (Socket -> IO Transport) -> Socket -> IO ServerEnv
initServerEnv serveState mkTransport serveSock = do
  clientList <- newIOHashMap
  workerList <- newIOHashMap
  pure ServerEnv{..}

serveForever
  :: MonadUnliftIO m
  => ServerT m ()
serveForever = do
  state <- asks serveState
  void . runMaybeT . forever $ do
    e <- lift tryServeOnce
    when (isLeft e) mzero
    alive <- readTVarIO state
    unless alive mzero

tryServeOnce
  :: MonadUnliftIO m
  => ServerT m (Either SomeException ())
tryServeOnce = tryAny serveOnce

serveOnce
  :: MonadUnliftIO m
  => ServerT m ()
serveOnce = do
  (sock', _) <- liftIO . accept =<< asks serveSock
  makeTransport <- asks mkTransport
  void $ async $ handleConnection =<< liftIO (makeTransport sock')

handleConnection
  :: MonadUnliftIO m
  => Transport -> ServerT m ()
handleConnection transport = do
  ServerEnv{..} <- ask
  schedEnv <- liftS ask
  connEnv <- liftIO $ initServerConnEnv transport

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
  => WorkerList -> TVar Int -> m ()
runCheckWorkerState ref alive = runCheckState "Worker" ref (checkWorkerState ref alive) alive

checkWorkerState :: MonadUnliftIO m =>  WorkerList -> TVar Int -> WorkerEnv -> m ()
checkWorkerState ref alive env0 = runWorkerT env0 $ do
  delay <- fromIntegral <$> readTVarIO alive
  expiredAt <- (delay +) <$> Worker.getLastVist
  now <- liftIO getEpochTime
  when (now > expiredAt) $ do
    Worker.close
    wid <- liftC connid
    HM.delete ref wid

runCheckClientState
  :: MonadUnliftIO m
  => ClientList -> TVar Int -> m ()
runCheckClientState ref alive = runCheckState "Client" ref (checkClientState ref alive) alive

checkClientState :: MonadIO m => ClientList -> TVar Int -> ClientEnv -> m ()
checkClientState ref alive env0 = runClientT env0 $ do
  delay <- fromIntegral <$> readTVarIO alive
  expiredAt <- (delay +) <$> Client.getLastVist
  now <- liftIO getEpochTime
  when (now > expiredAt) $ do
    Client.close
    cid <- liftC connid
    HM.delete ref cid

runCheckState
  :: MonadUnliftIO m
  => String -> IOHashMap a b -> (b -> m ()) -> TVar Int -> m ()
runCheckState var ref checkAlive alive = void . async . forever $ do
  delay <- readTVarIO alive
  liftIO $ threadDelay $ fromIntegral delay * 1000 * 1000
  mapM_ checkAlive =<< HM.elems ref
  size <- HM.size ref
  liftIO $ errorM "Periodic.Server" $ "Total " ++ var ++ ": " ++ show size


startServer :: (Socket -> IO Transport) -> FilePath -> Socket -> IO ()
startServer mk path sock = do
  state <- newTVarIO True
  sEnv <- initServerEnv state mk sock
  createDirectoryIfMissing True path
  sqlite <- initSQLite $ fromString $ path ++ "/data.sqlite"
  schedEnv <- initSchedEnv sqlite $ atomically $ writeTVar state False

  runSchedT schedEnv $ do
    startSchedT
    lift . runCheckClientState (clientList sEnv) =<< keepalive
    lift . runCheckWorkerState (workerList sEnv) =<< keepalive

    runServerT sEnv serveForever

    shutdown
    liftIO $ Socket.close sock
