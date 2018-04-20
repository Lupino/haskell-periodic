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

import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.Async.Lifted (async)
import           Control.Concurrent.STM.TVar
import           Control.Exception               (SomeException)
import           Control.Monad                   (forever, mzero, unless, void,
                                                  when)
import           Control.Monad.Base
import           Control.Monad.Catch             (MonadCatch, MonadMask,
                                                  MonadThrow, try)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Reader.Class      (MonadReader (ask), asks)
import           Control.Monad.STM               (atomically)
import           Control.Monad.Trans.Class       (MonadTrans, lift)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Maybe       (runMaybeT)
import           Control.Monad.Trans.Reader      (ReaderT, runReaderT)
import           Data.ByteString                 (ByteString)
import           Data.Either                     (isLeft)
import           Data.Int                        (Int64)
import           Network.Socket                  (Socket, accept)
import qualified Network.Socket                  as Socket (close)
import           Periodic.Connection
import qualified Periodic.Connection             as Conn
import           Periodic.IOHashMap              (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap              as HM
import           Periodic.Node                   (liftC)
import           Periodic.Server.Client
import qualified Periodic.Server.Client          as Client
import           Periodic.Server.Scheduler
import           Periodic.Server.Worker
import qualified Periodic.Server.Worker          as Worker
import           Periodic.Transport              (Transport)
import           Periodic.Types                  (ClientType (..), runParser)
import           Periodic.Utils                  (getEpochTime)
import           System.Log.Logger               (errorM)

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
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

instance MonadTrans ServerT where
  lift = ServerT . lift . lift

deriving instance MonadBase IO m => MonadBase IO (ServerT m)

instance MonadTransControl ServerT where
  type StT ServerT a = StT (ReaderT ServerEnv) (StT SchedT a)
  liftWith = defaultLiftWith2 ServerT unServerT
  restoreT = defaultRestoreT2 ServerT

instance MonadBaseControl IO m => MonadBaseControl IO (ServerT m) where
  type StM (ServerT m) a = ComposeSt ServerT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

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
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => ServerT m ()
serveForever = do
  state <- asks serveState
  void . runMaybeT . forever $ do
    e <- lift tryServeOnce
    when (isLeft e) mzero
    alive <- liftIO $ readTVarIO state
    unless alive mzero

tryServeOnce
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => ServerT m (Either SomeException ())
tryServeOnce = try serveOnce

serveOnce
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => ServerT m ()
serveOnce = do
  (sock', _) <- liftIO . accept =<< asks serveSock
  makeTransport <- asks mkTransport
  void $ async $ handleConnection =<< liftIO (makeTransport sock')

handleConnection
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
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
            liftIO $ HM.insert clientList cid clientEnv
            lift $ startClientT clientEnv
            liftIO $ HM.delete clientList cid
          Right TypeWorker -> do
            cid <- connid
            workerEnv <- lift $ initWorkerEnv connEnv schedEnv
            liftIO $ HM.insert workerList cid workerEnv
            lift $ startWorkerT workerEnv
            liftIO $ HM.delete workerList cid

  where receiveThen
          :: (MonadIO m, MonadCatch m)
          => (ByteString -> ConnectionT m ()) -> ConnectionT m ()
        receiveThen next = do
          e <- try receive
          case e of
            Left (_ :: SomeException) -> Conn.close
            Right pl                  -> next pl

        sendThen
          :: (MonadIO m, MonadCatch m)
          => ConnectionT m () -> ConnectionT m ()
        sendThen next = do
          e <- try $ send =<< connid
          case e of
            Left (_ :: SomeException) -> Conn.close
            Right _                   -> next

runCheckWorkerState
  :: (MonadIO m, MonadBaseControl IO m)
  => WorkerList -> TVar Int -> m ()
runCheckWorkerState ref alive = runCheckState "Worker" ref (checkWorkerState ref alive) alive

checkWorkerState :: MonadIO m =>  WorkerList -> TVar Int -> WorkerEnv -> m ()
checkWorkerState ref alive env0 = runWorkerT env0 $ do
  delay <- liftIO $ fromIntegral <$> readTVarIO alive
  expiredAt <- (delay +) <$> Worker.getLastVist
  now <- liftIO getEpochTime
  when (now > expiredAt) $ do
    Worker.close
    wid <- liftC connid
    liftIO $ HM.delete ref wid

runCheckClientState
  :: (MonadIO m, MonadBaseControl IO m)
  => ClientList -> TVar Int -> m ()
runCheckClientState ref alive = runCheckState "Client" ref (checkClientState ref alive) alive

checkClientState :: MonadIO m => ClientList -> TVar Int -> ClientEnv -> m ()
checkClientState ref alive env0 = runClientT env0 $ do
  delay <- liftIO $ fromIntegral <$> readTVarIO alive
  expiredAt <- (delay +) <$> Client.getLastVist
  now <- liftIO getEpochTime
  when (now > expiredAt) $ do
    Client.close
    cid <- liftC connid
    liftIO $ HM.delete ref cid

runCheckState
  :: (MonadIO m, MonadBaseControl IO m)
  => String -> IOHashMap a b -> (b -> m ()) -> TVar Int -> m ()
runCheckState var ref checkAlive alive = void . async . forever $ do
  delay <- liftIO $ readTVarIO alive
  liftIO $ threadDelay $ fromIntegral delay * 1000 * 1000
  mapM_ checkAlive =<< liftIO (HM.elems ref)
  size <- liftIO $ HM.size ref
  liftIO $ errorM "Periodic.Server" $ "Total " ++ var ++ ": " ++ show size


startServer :: (Socket -> IO Transport) -> FilePath -> Socket -> IO ()
startServer mk path sock = do
  state <- newTVarIO True
  sEnv <- initServerEnv state mk sock
  schedEnv <- initSchedEnv path $ atomically $ writeTVar state False

  runSchedT schedEnv $ do
    startSchedT
    lift . runCheckClientState (clientList sEnv) =<< keepalive
    lift . runCheckWorkerState (workerList sEnv) =<< keepalive

    runServerT sEnv serveForever

    shutdown
    liftIO $ Socket.close sock
