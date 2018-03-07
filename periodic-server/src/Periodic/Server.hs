{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Periodic.Server
  (
    startServer
  ) where

import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.Async.Lifted (async)
import           Control.Concurrent.STM.TVar
import           Control.Exception               (SomeException)
import           Control.Monad                   (forever, mzero, unless, void,
                                                  when)
import           Control.Monad.Catch             (MonadCatch, try)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.STM               (atomically)
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Control.Monad.Trans.Maybe       (runMaybeT)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State       (StateT, evalStateT, get, gets)
import           Data.ByteString                 (ByteString)
import           Data.Either                     (isLeft)
import           Data.Int                        (Int64)
import           Network.Socket                  (Socket, accept)
import qualified Network.Socket                  as Socket (close)
import           Periodic.Connection
import qualified Periodic.Connection             as Conn
import           Periodic.IOHashMap              (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap              as HM
import           Periodic.Server.Client
import qualified Periodic.Server.Client          as Client
import           Periodic.Server.Scheduler
import           Periodic.Server.Worker
import qualified Periodic.Server.Worker          as Worker
import           Periodic.Transport              (Transport)
import           Periodic.Types                  (ClientType (..), runParser)
import           Periodic.Utils                  (getEpochTime)
import           System.Log.Logger               (errorM)

type ClientList m = IOHashMap ByteString (ClientEnv m)
type WorkerList m = IOHashMap ByteString (WorkerEnv m)

data ServerConfig = ServerConfig
  { schedConfig :: SchedConfig
  , mkTransport :: Socket -> IO Transport
  , serveSock   :: Socket
  }

data ServerState m = ServerState
  { clientList :: ClientList m
  , workerList :: WorkerList m
  , schedState :: SchedState m
  , serveState :: TVar Bool
  }

type ServerT m = StateT (ServerState m) (ReaderT ServerConfig (SchedT m))

runServerT :: Monad m => ServerState m -> ServerConfig -> ServerT m a -> m a
runServerT serverState serverConfig =
  runSchedT (schedState serverState) (schedConfig serverConfig) .
    flip runReaderT serverConfig . flip evalStateT serverState

runSchedT' :: Monad m => SchedT m a -> ServerT m a
runSchedT' = lift . lift

initServerConfig :: SchedConfig -> (Socket -> IO Transport) -> Socket -> ServerConfig
initServerConfig = ServerConfig

initServerState :: TVar Bool -> IO (ServerState m)
initServerState serveState = do
  clientList <- newIOHashMap
  workerList <- newIOHashMap
  schedState <- initSchedState
  pure ServerState{..}

serveForever
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => ServerT m ()
serveForever = do
  runSchedT' startSchedT

  liftS4 . flip runCheckClientState 100 =<< gets clientList
  liftS4 . flip runCheckWorkerState 100 =<< gets workerList

  state <- gets serveState

  void . runMaybeT . forever $ do
    e <- lift tryServeOnce
    when (isLeft e) mzero
    alive <- liftIO $ readTVarIO state
    unless alive mzero


  runSchedT' shutdown
  liftIO . Socket.close =<< lift (asks serveSock)

tryServeOnce
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => ServerT m (Either SomeException ())
tryServeOnce = try serveOnce

serveOnce
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => ServerT m ()
serveOnce = do
  (sock', _) <- liftIO . accept =<< lift (asks serveSock)
  makeTransport <- lift (asks mkTransport)
  void $ async $ handleConnection =<< liftIO (makeTransport sock')

handleConnection
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => Transport -> ServerT m ()
handleConnection transport = do
  connectionConfig <- liftIO $ initServerConnectionConfig transport
  connectionState <- liftIO initConnectionState

  ServerState{..} <- get
  ServerConfig{..} <- lift ask

  lift . lift . runConnectionT connectionState connectionConfig $
    receiveThen $ \pl ->
      sendThen $
        case runParser pl of
          Left _           -> Conn.close
          Right TypeClient -> do
            cid <- connid
            clientEnv <- liftC4 $ initClientEnv
                connectionState connectionConfig schedState schedConfig
            liftIO $ HM.insert clientList cid clientEnv
            liftC4 $ startClientT clientEnv
            liftIO $ HM.delete clientList cid
          Right TypeWorker -> do
            cid <- connid
            workerEnv <- liftC4 $ initWorkerEnv
                connectionState connectionConfig schedState schedConfig
            liftIO $ HM.insert workerList cid workerEnv
            liftC4 $ startWorkerT workerEnv
            liftIO $ HM.delete workerList cid

  where receiveThen
          :: (MonadIO m, MonadCatch m)
          => (ByteString -> ConnectionT (SchedT m) ()) -> ConnectionT (SchedT m) ()
        receiveThen next = do
          e <- try receive
          case e of
            Left (_ :: SomeException) -> Conn.close
            Right pl                  -> next pl

        sendThen
          :: (MonadIO m, MonadCatch m)
          => ConnectionT (SchedT m) () -> ConnectionT (SchedT m) ()
        sendThen next = do
          e <- try $ send =<< connid
          case e of
            Left (_ :: SomeException) -> Conn.close
            Right _                   -> next

liftC4 :: Monad m => m a -> ConnectionT (SchedT m) a
liftC4 = lift . lift . lift . lift

liftS4 :: Monad m => m a -> ServerT m a
liftS4 = lift . lift . lift . lift

runCheckWorkerState
  :: (MonadIO m, MonadBaseControl IO m)
  => WorkerList m -> Int64 -> m ()
runCheckWorkerState ref alive = runCheckState "Worker" ref (checkWorkerState ref alive) alive

checkWorkerState :: MonadIO m =>  WorkerList m -> Int64 -> WorkerEnv m -> m ()
checkWorkerState ref alive env0 = runWorkerT env0 $ do
  expiredAt <- (alive +) <$> Worker.getLastVist
  now <- liftIO getEpochTime
  when (now > expiredAt) $ do
    Worker.close
    wid <- lift $ lift connid
    liftIO $ HM.delete ref wid

runCheckClientState
  :: (MonadIO m, MonadBaseControl IO m)
  => ClientList m -> Int64 -> m ()
runCheckClientState ref alive = runCheckState "Client" ref (checkClientState ref alive) alive

checkClientState :: MonadIO m => ClientList m -> Int64 -> ClientEnv m -> m ()
checkClientState ref alive env0 = runClientT env0 $ do
  expiredAt <- (alive +) <$> Client.getLastVist
  now <- liftIO getEpochTime
  when (now > expiredAt) $ do
    Client.close
    cid <- lift $ lift connid
    liftIO $ HM.delete ref cid

runCheckState
  :: (MonadIO m, MonadBaseControl IO m)
  => String -> IOHashMap a b -> (b -> m ()) -> Int64 -> m ()
runCheckState var ref checkAlive alive = void . async . forever $ do
  liftIO $ threadDelay $ fromIntegral alive * 1000 * 1000
  mapM_ checkAlive =<< liftIO (HM.elems ref)
  size <- liftIO $ HM.size ref
  liftIO $ errorM "Periodic.Server" $ "Total " ++ var ++ ": " ++ show size


startServer :: (Socket -> IO Transport) -> FilePath -> Socket -> IO ()
startServer mk path sock = do
  state <- newTVarIO True
  schedConfig <- initSchedConfig path $ atomically $ writeTVar state False
  let serverConfig = initServerConfig schedConfig mk sock

  serverState <- initServerState state
  runServerT serverState serverConfig serveForever
