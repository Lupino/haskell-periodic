{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Periodic.Trans.Worker
  ( WorkerT
  , startWorkerT
  , startWorkerTWithSignal
  , ping
  , addFunc
  , broadcast
  , removeFunc
  , work
  , close
  , runJobT
  , getClientEnv
  ) where


import           Control.Monad                (forever, replicateM, unless,
                                               void, when)
import           Control.Monad.Reader.Class   (MonadReader, asks)
import           Control.Monad.Trans.Class    (MonadTrans, lift)
import           Control.Monad.Trans.Reader   (ReaderT (..), runReaderT)
import           Data.Binary                  (Binary)
import           Data.Binary.Get              (getWord32be, runGetOrFail)
import qualified Data.ByteString.Lazy         as BL (fromStrict, null)
import           Data.Int                     (Int64)
import           Data.IOMap                   (IOMap)
import qualified Data.IOMap                   as Map (delete, empty,
                                                      foldrWithKey', insert,
                                                      lookup)
import qualified Data.IOMap.STM               as MapS (modifyIOMap, toList)
import qualified Data.Map.Strict              as RMap (empty)
import           Metro.Class                  (RecvPacket (..), Transport,
                                               TransportConfig, getPacketId)
import           Metro.Conn                   (initConnEnv, runConnT)
import qualified Metro.Conn                   as Conn
import           Metro.Node                   (NodeMode (..), SessionMode (..),
                                               initEnv1, nextSessionId,
                                               setDefaultSessionTimeout1,
                                               setNodeMode, setSessionMode,
                                               startNodeT_, withEnv,
                                               withSessionT_)
import           Metro.Session                (receive, send)
import           Metro.Utils                  (foreverExit, getEpochTime)
import           Periodic.Node
import qualified Periodic.Trans.BaseClient    as BT (BaseClientEnv, close,
                                                     getClientEnv, ping,
                                                     successRequest)
import           Periodic.Trans.Job           (JobT, func_, name, workFail)
import           Periodic.Types               (ClientType (TypeWorker), Msgid,
                                               Packet, getClientType, getResult,
                                               packetREQ, recvRawPacket,
                                               regPacketREQ)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand
import           System.Log.Logger            (errorM)
import           UnliftIO
import           UnliftIO.Concurrent          (threadDelay)

type TaskList tp m = IOMap FuncName (JobT tp m ())

type JobList = IOMap (Msgid, JobHandle) Job

type GrabList = IOMap Msgid Int64

instance (Binary a) => RecvPacket (Maybe Job) (Packet a) where
  recvPacket _ = recvRawPacket

data WorkerEnv tp m = WorkerEnv
    { taskList       :: TaskList tp m
    , jobList        :: JobList
    , tskSizeH       :: TVar Int
    , maxSizeH       :: TVar Int
    , grabList       :: GrabList
    , queueInFlightH :: TVar Int
    , shutdownH      :: Maybe (TVar Bool)
    }

newtype WorkerT tp m a = WorkerT {unWorkerT :: ReaderT (WorkerEnv tp m) (JobT tp m) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (WorkerEnv tp m)
    )

instance MonadUnliftIO m => MonadUnliftIO (WorkerT tp m) where
  withRunInIO inner = WorkerT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runWorkerT r)

instance MonadTrans (WorkerT tp) where
  lift = WorkerT . lift . lift

runWorkerT :: WorkerEnv tp m -> WorkerT tp m a -> JobT tp m a
runWorkerT workerEnv = flip runReaderT workerEnv . unWorkerT

startWorkerT
  :: (MonadUnliftIO m, Transport tp)
  => TransportConfig tp -> WorkerT tp m () -> m ()
startWorkerT = startWorkerTWithSignal Nothing $ pure ()

getShutdown :: MonadIO m => Maybe (TVar Bool) -> m Bool
getShutdown = maybe (pure False) readTVarIO

startWorkerTWithSignal
  :: (MonadUnliftIO m, Transport tp)
  => Maybe (TVar Bool) -> m () -> TransportConfig tp -> WorkerT tp m () -> m ()
startWorkerTWithSignal mShutdownH cleanup config m = foreverExit $ \exit -> do
  shouldStop <- getShutdown mShutdownH
  when shouldStop $ exit ()

  e <- lift $ tryAny (startWorkerT_ mShutdownH cleanup config m)
  shouldStopRetry <- getShutdown mShutdownH
  when shouldStopRetry $ exit ()

  let extMsg = case e of
                 Left err -> " Error: " ++ show err
                 Right _  -> ""


  liftIO $ errorM "Periodic.Trans.Worker" $ "Worker disconnected, retrying in 3s." ++ extMsg
  lift $ threadDelay reconnectDelayUs
  where
    reconnectDelayUs = 3 * 1000 * 1000

startWorkerT_
  :: (MonadUnliftIO m, Transport tp)
  => Maybe (TVar Bool) -> m () -> TransportConfig tp -> WorkerT tp m () -> m ()
startWorkerT_ shutdownH cleanup config m = do
  connEnv <- initConnEnv config
  r <- runConnT connEnv $ do
    Conn.send $ regPacketREQ TypeWorker
    Conn.receive_

  let nid = case getClientType r of
              Data v ->
                case runGetOrFail getWord32be (BL.fromStrict v) of
                  Right (rest, _, nidV) | BL.null rest -> nidV
                  _                                    -> 0
              _      -> 0

  taskList <- Map.empty
  jobList  <- Map.empty
  tskSizeH <- newTVarIO 0
  maxSizeH <- newTVarIO 0
  grabList <- Map.empty
  queueInFlightH <- newTVarIO 0

  jobEnv1 <- initEnv1 mapEnv connEnv Nothing (Nid nid) True sessionGen
  setDefaultSessionTimeout1 jobEnv1 100

  let wEnv = WorkerEnv {..}

  runNodeT jobEnv1 $ do
    aNode <- async $ startNodeT_ (filterPacketM jobList) defaultSessionHandler
    aHealth <- async $ runWorkerT wEnv workerHealthLoop
    aQueue <- async $ runWorkerT wEnv $ runJobT $ processJobQueue wEnv
    aMain <- async $ runWorkerT wEnv m
    (_, e) <- waitAnyCatch [aNode, aHealth, aQueue, aMain]
    isShuttingDown <- getShutdown shutdownH
    when isShuttingDown $ runWorkerT wEnv $ gracefulExit cleanup
    mapM_ cancel [aNode, aHealth, aQueue, aMain]
    either throwIO pure e

  where mapEnv =
          setNodeMode Multi
          . setSessionMode SingleAction
        healthCheckIntervalUs = 5 * 1000 * 1000

        workerHealthLoop :: (MonadUnliftIO m, Transport tp) => WorkerT tp m ()
        workerHealthLoop = forever $ do
          threadDelay healthCheckIntervalUs
          ret <- timeout 10000000 ping
          case ret of
            Just True  -> pure ()
            Just False -> throwString "worker health check failed: ping returned False"
            Nothing    -> throwString "worker health check failed: ping timeout"

filterPacketM :: MonadIO m => JobList -> Packet ServerCommand -> m Bool
filterPacketM jl rpkt = do
  case getResult Nothing getAssignJob (Just rpkt) of
    Nothing  -> return True
    Just job -> do
      Map.insert (getPacketId rpkt, getHandle job) job jl
      return False


runJobT :: Monad m => JobT tp m a -> WorkerT tp m a
runJobT = WorkerT . lift

getClientEnv
  :: (Monad m, Transport tp)
  => WorkerT tp m (BT.BaseClientEnv (Maybe Job) tp)
getClientEnv = runJobT BT.getClientEnv

close :: (MonadUnliftIO m, Transport tp) => WorkerT tp m ()
close = runJobT BT.close

ping :: (MonadUnliftIO m, Transport tp) => WorkerT tp m Bool
ping = runJobT BT.ping

addFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobT tp m () -> WorkerT tp m Bool
addFunc f j = do
  r <- runJobT $ BT.successRequest (packetREQ $ CanDo f)
  ref <- asks taskList
  Map.insert f j ref
  pure r

broadcast
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobT tp m () -> WorkerT tp m Bool
broadcast f j = do
  r <- runJobT $ BT.successRequest (packetREQ $ Broadcast f)
  ref <- asks taskList
  Map.insert f j ref
  pure r

removeFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> WorkerT tp m Bool
removeFunc f = do
  tskList <- asks taskList
  runJobT $ removeFunc_ tskList f

removeFunc_
  :: (MonadUnliftIO m, Transport tp)
  => TaskList tp m -> FuncName -> JobT tp m Bool
removeFunc_ ref f = do
  r <- BT.successRequest (packetREQ $ CantDo f)
  Map.delete f ref
  return r

removeFuncServerOnly_
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobT tp m Bool
removeFuncServerOnly_ f = BT.successRequest (packetREQ $ CantDo f)

getAssignJob :: ServerCommand -> Maybe Job
getAssignJob (JobAssign job) = Just job
getAssignJob _               = Nothing

sendGrab :: (Transport tp, MonadUnliftIO m) => Msgid -> JobT tp m Bool
sendGrab msgid = do
  withSessionT_ (pure msgid) (Just 10) $ do
    send (packetREQ GrabJob)
    getResult False isSuccess <$> receive

nextGrab :: (Transport tp, MonadUnliftIO m) => GrabList -> JobT tp m ()
nextGrab gl = do
  v <- Map.foldrWithKey' foldFunc Nothing gl
  case v of
    Nothing -> pure ()
    Just (msgid, ts) -> do
      now <- getEpochTime
      when (ts + 300 < now) $ do
        ok <- sendGrab msgid
        unless ok $
          throwString "sendGrab failed: server connection may be broken"
        Map.insert msgid now gl

  where foldFunc :: Msgid -> Int64 -> Maybe (Msgid, Int64) -> Maybe (Msgid, Int64)
        foldFunc msgid ts Nothing = Just (msgid, ts)
        foldFunc msgid ts (Just (msgid0, ts0))
          | ts0 > ts  = Just (msgid, ts)
          | otherwise = Just (msgid0, ts0)

work
  :: (MonadUnliftIO m, Transport tp)
  => Int -> WorkerT tp m ()
work size = do
  maxSize <- asks maxSizeH
  tskSize <- asks tskSizeH
  gl      <- asks grabList
  mShutdownH <- asks shutdownH

  atomically $ writeTVar maxSize size

  runJobT $ do
    mapM_ (\msgid -> Map.insert msgid 0 gl) =<< replicateM size nextSessionId
    foreverExit $ \exit -> do
      isShuttingDown <- getShutdown mShutdownH
      when isShuttingDown $ exit ()
      s <- readTVarIO tskSize
      maxSize' <- readTVarIO maxSize
      when (s < maxSize') $ lift $ nextGrab gl
      waitForNextRound mShutdownH
  where
    -- Keep the original ~60s grab cadence, but wake early on shutdown.
    waitForNextRound mShutdownH = do
      timer <- liftIO $ registerDelay 60000000
      atomically $ do
        timeoutReached <- readTVar timer
        isShuttingDown <- maybe (pure False) readTVar mShutdownH
        unless (timeoutReached || isShuttingDown) retrySTM

gracefulExit
  :: (MonadUnliftIO m, Transport tp)
  => m () -> WorkerT tp m ()
gracefulExit cleanup = do
  tskList <- asks taskList
  funcs <- atomically $ map fst <$> MapS.toList tskList
  runJobT $ mapM_ removeFuncServerOnly_ funcs
  -- New jobs will be refused in processJob with JobUnassigned once
  -- shutdownH is set.
  liftIO $ errorM "Periodic.Trans.Worker" "Got shutdown signal."
  io <- async $ forever $ do
    rt <- runningTaskCount
    liftIO $ errorM "Periodic.Trans.Worker" $ "State: shuttingDown=True, runningTasks=" ++ show rt
    threadDelay 1000000
  waitIdle
  lift cleanup
  -- close

  cancel io

runningTaskCount :: MonadIO m => WorkerT tp m Int
runningTaskCount = do
  ref <- asks tskSizeH
  readTVarIO ref

waitIdle :: MonadIO m => WorkerT tp m ()
waitIdle = do
  runningRef <- asks tskSizeH
  inFlightRef <- asks queueInFlightH
  jobsRef <- asks jobList
  atomically $ do
    n <- readTVar runningRef
    q <- readTVar inFlightRef
    pending <- MapS.toList jobsRef
    when (n > 0 || q > 0 || not (null pending)) retrySTM


processJob :: (MonadUnliftIO m, Transport tp) => WorkerEnv tp m -> ((Msgid, JobHandle), Job) -> JobT tp m ()
processJob WorkerEnv{..} ((sid, _), job) = do
  isAccepted <- atomically $ do
    isShuttingDown <- maybe (pure False) readTVar shutdownH
    s <- readTVar tskSizeH
    maxSize <- readTVar maxSizeH
    if isShuttingDown || maxSize <= s
      then pure False
      else do
        writeTVar tskSizeH (s + 1)
        pure True

  withSessionT_ (pure sid) (Just 10) $
    send (packetREQ $ if isAccepted then JobAssigned else JobUnassigned)

  when isAccepted $ do
    nextGrab grabList
    processJob_ taskList job `finally` finishJob tskSizeH shutdownH sid

finishJob :: (MonadUnliftIO m, Transport tp) => TVar Int -> Maybe (TVar Bool) -> Msgid -> JobT tp m ()
finishJob tskSizeH mShutdownH sid = do
  atomically $ do
    s <- readTVar tskSizeH
    writeTVar tskSizeH (s - 1)
  shouldGrab <- not <$> getShutdown mShutdownH
  when shouldGrab $
    void $ sendGrab sid

processJob_ :: (MonadUnliftIO m, Transport tp) => TaskList tp m -> Job -> JobT tp m ()
processJob_ taskList job = withEnv (Just job) $ do
  f <- func_
  task <- Map.lookup f taskList
  case task of
    Nothing -> do
      void $ removeFunc_ taskList f
      void workFail
    Just task' ->
      catchAny task' $ \e -> do
        n <- name
        liftIO
          $ errorM "Periodic.Trans.Worker"
          $ concat
          [ "Failing on running job { name = "
          , n
          , ", "
          , show f
          , " }"
          , "\nError: "
          , show e
          ]
        void workFail

processJobQueue :: (MonadUnliftIO m, Transport tp) => WorkerEnv tp m -> JobT tp m ()
processJobQueue wEnv@WorkerEnv {..} = forever $ do
  jobs <- atomically $ do
    v <- MapS.toList jobList
    if null v then retrySTM
              else do
                MapS.modifyIOMap (const RMap.empty) jobList
                return v

  mapM_ (\j -> do
            atomically $ do
              q <- readTVar queueInFlightH
              writeTVar queueInFlightH (q + 1)
            void $ async $
              processJob wEnv j `finally` atomically (do
                q <- readTVar queueInFlightH
                writeTVar queueInFlightH (q - 1)))
        jobs
