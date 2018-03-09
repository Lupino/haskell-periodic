{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Periodic.Server.Scheduler
  (
    SchedT
  , runSchedT
  , SchedState
  , SchedConfig
  , initSchedState
  , initSchedConfig
  , startSchedT
  , pushJob
  , pushGrab
  , failJob
  , doneJob
  , schedLaterJob
  , addFunc
  , removeFunc
  , broadcastFunc
  , dropFunc
  , removeJob
  , dumpJob
  , status
  , shutdown
  ) where

import           Control.Exception               (SomeException, try)
import           Control.Monad                   (forever, mzero, unless, void,
                                                  when)
import           Data.Binary                     (decodeFile, encodeFile)
import           Data.Int                        (Int64)
import           Data.Maybe                      (fromJust, fromMaybe, isJust)
import           Periodic.Agent                  (AgentEnv, aAlive,
                                                  runAgentTWithEnv, send)
import           Periodic.IOHashMap              (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap              as FL
import           Periodic.IOList                 (IOList)
import qualified Periodic.IOList                 as IL
import qualified Periodic.Lock                   as L (Lock, new, with)
import           Periodic.Server.FuncStat
import           Periodic.Server.GrabQueue
import           Periodic.Server.JobQueue        (JobQueue)
import qualified Periodic.Server.JobQueue        as JQ
import           Periodic.Server.ProcessQueue    (ProcessQueue)
import qualified Periodic.Server.ProcessQueue    as PQ
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand    (ServerCommand (JobAssign))
import           Periodic.Utils                  (getEpochTime)

import           System.Directory                (createDirectoryIfMissing,
                                                  doesFileExist)
import           System.FilePath                 ((</>))

import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.Async.Lifted (Async, async, cancel, poll)
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM               (atomically, retry)

import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Control     (MonadBaseControl, StM)
import           Control.Monad.Trans.Maybe       (runMaybeT)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State       (StateT, evalStateT, get, gets)

data SchedConfig = SchedConfig
  { sSchedDelay :: Int64
  , sStorePath  :: FilePath
  , sCleanup    :: IO ()
  }

type Task m = Async (StM (SchedT m) ())

data SchedState m = SchedState
  { sFuncStatList :: FuncStatList
  , sLocker       :: L.Lock
  , sGrabQueue    :: GrabQueue
  , sJobQueue     :: JobQueue
  , sProcessJob   :: ProcessQueue
  , sAlive        :: TVar Bool
  , sTaskList     :: IOHashMap JobHandle (Task m)
  }

type SchedT m = StateT (SchedState m) (ReaderT SchedConfig m)

runSchedT :: Monad m => SchedState m -> SchedConfig -> SchedT m a -> m a
runSchedT schedState schedConfig =
  flip runReaderT schedConfig . flip evalStateT schedState

initSchedConfig :: FilePath -> IO () -> IO SchedConfig
initSchedConfig path sCleanup = do
  createDirectoryIfMissing True path
  pure SchedConfig{..}
  where sSchedDelay = 10
        sStorePath = path </> "dump.db"

initSchedState :: IO (SchedState m)
initSchedState = do
  sFuncStatList <- newIOHashMap
  sLocker       <- L.new
  sGrabQueue    <- newGrabQueue
  sJobQueue     <- newIOHashMap
  sProcessJob   <- newIOHashMap
  sTaskList    <- newIOHashMap
  sAlive        <- newTVarIO True
  pure SchedState {..}

startSchedT :: (MonadIO m, MonadBaseControl IO m) => SchedT m ()
startSchedT = do
  SchedConfig{..} <- lift ask
  SchedState{..} <- get
  runTask 300 revertProcessQueue
  runTask 300 saveJob
  runTask (fromIntegral sSchedDelay) pollJob
  exists <- liftIO $ doesFileExist sStorePath
  when exists $ mapM_ pushJob =<< loadJob
  mapM_ adjustFuncStat =<< liftIO (FL.keys sJobQueue)

runTask :: (MonadIO m, MonadBaseControl IO m) => Int -> SchedT m () -> SchedT m ()
runTask delay m = void . async $ do
  SchedState{..} <- get
  void . runMaybeT . forever $ do
    liftIO $ threadDelay $ delay * 1000 * 1000
    alive <- liftIO $ readTVarIO sAlive
    if alive then lift m
             else mzero

pollJob :: (MonadIO m, MonadBaseControl IO m) => SchedT m ()
pollJob = do
  SchedState{..} <- get
  SchedConfig{..} <- lift ask
  mapM_ checkPoll =<< liftIO (FL.toList sTaskList)

  next <- liftIO $ (+ sSchedDelay * 2) <$> getEpochTime
  mapM_ checkJob =<< liftIO (JQ.findLessJob sJobQueue next)

  where checkJob
          :: (MonadIO m, MonadBaseControl IO m)
          => Job -> SchedT m ()
        checkJob job@Job{..} = do
          w <- findTask job
          schedDelay <- lift $ asks sSchedDelay
          unless (isJust w) $ do
            now <- liftIO getEpochTime
            when (jSchedAt > now || jSchedAt + schedDelay < now) $ reSchedJob job

        checkPoll
          :: (MonadIO m, MonadBaseControl IO m)
          => (JobHandle, Async (StM (SchedT m) ())) -> SchedT m ()
        checkPoll (jh, w) = do
          taskList <- gets sTaskList
          r <- poll w
          case r of
            Just (Right ()) -> liftIO $ FL.delete taskList jh
            _               -> pure ()


pushJob :: (MonadIO m, MonadBaseControl IO m) => Job -> SchedT m ()
pushJob job@Job{..} = do
  SchedState{..} <- get
  exists <- liftIO $ JQ.memberJob sJobQueue jFuncName jName
  isProc <- liftIO $ PQ.memberJob sProcessJob jFuncName (hashJobName jName)
  if exists then doPushJob
            else unless isProc doPushJob

  adjustFuncStat jFuncName

  where doPushJob :: (MonadIO m, MonadBaseControl IO m) => SchedT m ()
        doPushJob = do
          queue <- gets sJobQueue
          reSchedJob job
          liftIO $ JQ.pushJob queue job

reSchedJob :: (MonadIO m, MonadBaseControl IO m) => Job -> SchedT m ()
reSchedJob job = do
  schedDelay <- lift $ asks sSchedDelay
  taskList <- gets sTaskList
  w <- findTask job
  when (isJust w) $ do
    cancel (fromJust w)
    liftIO $ FL.delete taskList (jHandle job)

  next <- liftIO $ (+ schedDelay * 2) <$> getEpochTime
  when (jSchedAt job < next) $ do
    w' <- schedJob job
    liftIO $ FL.insert taskList (jHandle job) w'

findTask :: (MonadIO m) =>  Job -> SchedT m (Maybe (Task m))
findTask job = do
  taskList <- gets sTaskList
  liftIO $ FL.lookup taskList (jHandle job)

schedJob
  :: (MonadIO m, MonadBaseControl IO m)
  => Job -> SchedT m (Task m)
schedJob job = async $ schedJob_ job

schedJob_ :: MonadIO m => Job -> SchedT m ()
schedJob_ job@Job{..} = do
  SchedState{..} <- get
  now <- liftIO getEpochTime
  when (jSchedAt > now) . liftIO . threadDelay . fromIntegral $ (jSchedAt - now) * 1000000
  FuncStat{..} <- liftIO . atomically $ do
    st <- FL.lookupSTM sFuncStatList jFuncName
    case st of
      Nothing                  -> retry
      Just FuncStat{sWorker=0} -> retry
      Just st'                 -> pure st'
  if sBroadcast then popAgentListThen $ doneSubmitJob True
                else popAgentThen $ doneSubmitJob False

  where popAgentThen :: MonadIO m => (AgentEnv -> SchedT m ()) -> SchedT m ()
        popAgentThen done = do
          SchedState{..} <- get
          (jq, env0) <- liftIO $ atomically $ popAgentSTM sGrabQueue jFuncName
          alive <- liftIO $ runAgentTWithEnv env0 aAlive
          when alive $ do
            liftIO $ IL.insert jq (jHandle job)
            done env0

        popAgentListThen :: MonadIO m => (AgentEnv -> SchedT m ()) -> SchedT m ()
        popAgentListThen done = do
          SchedState{..} <- get
          agents <- liftIO $ popAgentList sGrabQueue jFuncName
          mapM_ (done . snd) agents
          unless (null agents) endSchedJob

        doneSubmitJob :: MonadIO m => Bool -> AgentEnv -> SchedT m ()
        doneSubmitJob cast agent = do
          SchedState{..} <- get
          unless cast . liftIO $ do
            nextSchedAt <- getEpochTime
            PQ.insertJob sProcessJob job { jSchedAt = nextSchedAt }

          e <- liftIO . try $ assignJob agent job
          case e of
            Left (_::SomeException) ->
              unless cast $ liftIO $
                PQ.removeJob sProcessJob jFuncName (hashJobName jName)
            Right _ -> do
              adjustFuncStat jFuncName
              endSchedJob

        endSchedJob :: MonadIO m => SchedT m ()
        endSchedJob = do
          SchedState{..} <- get
          liftIO $ do
            JQ.removeJob sJobQueue jFuncName jName
            FL.delete sTaskList (jHandle job)

adjustFuncStat :: MonadIO m => FuncName -> SchedT m ()
adjustFuncStat fn = do
  SchedState{..} <- get
  liftIO . L.with sLocker $ do
    size <- fromIntegral <$> JQ.sizeJob sJobQueue fn
    sizePQ <- fromIntegral <$> PQ.sizeJob sProcessJob fn
    schedAt <- do
      minJob <- JQ.findMinJob sJobQueue fn
      case minJob of
        Nothing  -> getEpochTime
        Just job -> return $ jSchedAt job
    FL.alter sFuncStatList (update (size + sizePQ) sizePQ schedAt) fn

  where update :: Int64 -> Int64 -> Int64 -> Maybe FuncStat -> Maybe FuncStat
        update size sizePQ schedAt st =
          Just ((fromMaybe (funcStat fn) st) { sJob = size
                                             , sProcess = sizePQ
                                             , sSchedAt = schedAt
                                             })

removeJob :: (MonadIO m, MonadBaseControl IO m) => Job -> SchedT m ()
removeJob job = do
  SchedState{..} <- get
  liftIO $ do
    has <- JQ.memberJob sJobQueue (jFuncName job) (jName job)
    when has $ JQ.removeJob sJobQueue (jFuncName job) (jName job)

    isProc <- PQ.memberJob sProcessJob (jFuncName job) (hashJobName $ jName job)
    when isProc $ PQ.removeJob sProcessJob (jFuncName job) (hashJobName $ jName job)

  adjustFuncStat (jFuncName job)

  w <- findTask job
  when (isJust w) $ do
    cancel (fromJust w)
    liftIO $ FL.delete sTaskList (jHandle job)

dumpJob :: MonadIO m => SchedT m [Job]
dumpJob = do
  js <- liftIO . JQ.dumpJob =<< gets sJobQueue
  js' <- liftIO . PQ.dumpJob =<< gets sProcessJob
  return $ js ++ js'

saveJob :: MonadIO m => SchedT m ()
saveJob = do
  path <- lift $ asks sStorePath
  dumpJob >>= liftIO . encodeFile path

loadJob :: MonadIO m => SchedT m [Job]
loadJob = liftIO . decodeFile =<< lift (asks sStorePath)

addFunc :: MonadIO m => FuncName -> SchedT m ()
addFunc n = broadcastFunc n False

broadcastFunc :: MonadIO m => FuncName -> Bool -> SchedT m ()
broadcastFunc n cast = do
  SchedState{..} <- get
  liftIO . L.with sLocker $ FL.alter sFuncStatList updateStat n

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just ((funcStat n) {sWorker = 1, sBroadcast = cast})
        updateStat (Just fs) = Just (fs { sWorker = sWorker fs + 1, sBroadcast = cast })

removeFunc :: MonadIO m => FuncName -> SchedT m ()
removeFunc n = do
  SchedState{..} <- get
  liftIO . L.with sLocker $ FL.alter sFuncStatList updateStat n

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just (funcStat n)
        updateStat (Just fs) = Just (fs { sWorker = max (sWorker fs - 1) 0 })

dropFunc :: MonadIO m => FuncName -> SchedT m ()
dropFunc n = do
  SchedState{..} <- get
  liftIO . L.with sLocker $ do
    st <- FL.lookup sFuncStatList n
    when (isJust st) $
      when (sWorker (fromJust st) == 0) $ do
        FL.delete sFuncStatList n
        FL.delete sJobQueue n

pushGrab :: MonadIO m => IOList FuncName -> IOList JobHandle -> AgentEnv -> SchedT m ()
pushGrab funcList handleList ag = do
  queue <- gets sGrabQueue
  liftIO $ pushAgent queue funcList handleList ag

assignJob :: AgentEnv -> Job -> IO ()
assignJob env0 job =
  runAgentTWithEnv env0 $ send (JobAssign (jHandle job) job)

failJob :: (MonadIO m, MonadBaseControl IO m) => JobHandle -> SchedT m ()
failJob jh = do
  SchedState{..} <- get
  job <- liftIO $ PQ.lookupJob sProcessJob fn jn
  when (isJust job) $ do
    nextSchedAt <- liftIO getEpochTime
    retryJob ((fromJust job) {jSchedAt = nextSchedAt})

  where (fn, jn) = unHandle jh

retryJob :: (MonadIO m, MonadBaseControl IO m) => Job -> SchedT m ()
retryJob job = do
  SchedState{..} <- get
  liftIO $ JQ.pushJob sJobQueue job
  liftIO $ PQ.removeJob sProcessJob fn (hashJobName jn)

  adjustFuncStat fn
  reSchedJob job

  where  fn = jFuncName job
         jn = jName job


doneJob :: MonadIO m => JobHandle -> SchedT m ()
doneJob jh = do
  SchedState{..} <- get
  job <- liftIO $ PQ.lookupJob sProcessJob fn jn
  when (isJust job) $ do
    liftIO $ PQ.removeJob sProcessJob fn jn
    adjustFuncStat fn

  where (fn, jn) = unHandle jh

schedLaterJob
  :: (MonadIO m, MonadBaseControl IO m)
  => JobHandle -> Int64 -> Int -> SchedT m ()
schedLaterJob jh later step = do
  SchedState{..} <- get
  job <- liftIO $ PQ.lookupJob sProcessJob fn jn
  when (isJust job) $ do
    let job' = fromJust job

    nextSchedAt <- liftIO $ (+) later <$> getEpochTime
    retryJob job' {jSchedAt = nextSchedAt , jCount = jCount job' + step}

  where (fn, jn) = unHandle jh

status :: MonadIO m => SchedT m [FuncStat]
status = liftIO . FL.elems =<< gets sFuncStatList

revertProcessQueue :: (MonadIO m, MonadBaseControl IO m) => SchedT m ()
revertProcessQueue = do
  now <- liftIO getEpochTime
  queue <- gets sProcessJob
  mapM_ (failJob . jHandle)
    =<< filter (isTimeout now) <$> liftIO (PQ.dumpJob queue)
  where isTimeout :: Int64 -> Job -> Bool
        isTimeout t1 Job{jSchedAt = t} = (t + 600) < t1

shutdown :: (MonadIO m, MonadBaseControl IO m) => SchedT m ()
shutdown = do
  SchedState{..} <- get
  SchedConfig{..} <- lift ask
  alive <- liftIO $ atomically $ do
    t <- readTVar sAlive
    writeTVar sAlive False
    return t
  when alive $ do
    mapM_ cancel =<< liftIO (FL.elems sTaskList)
    saveJob
    void . async $ liftIO sCleanup
