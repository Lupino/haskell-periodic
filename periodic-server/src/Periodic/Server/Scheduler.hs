{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Periodic.Server.Scheduler
  (
    SchedT
  , runSchedT
  , SchedEnv
  , initSchedEnv
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
import           Periodic.Agent                  (AgentEnv', aAlive, runAgentT',
                                                  send)
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

import           Control.Monad.Base
import           Control.Monad.Catch             (MonadCatch, MonadMask,
                                                  MonadThrow)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Reader.Class      (MonadReader (ask), asks)
import           Control.Monad.Trans.Class       (MonadTrans, lift)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Maybe       (runMaybeT)
import           Control.Monad.Trans.Reader      (ReaderT, runReaderT)

import           System.IO                       (readFile, writeFile)
import           Text.Read                       (readMaybe)

data Action = Add Job | Remove Job | Cancel | PollJob

data SchedEnv = SchedEnv
  { sPollDelay    :: TVar Int -- main poll loop every time delay
  , sRevertDelay  :: TVar Int -- revert process queue loop every time delay
  , sTaskTimeout  :: TVar Int -- the task do timeout
  , sStorePath    :: FilePath
  , sCleanup      :: IO ()
  , sFuncStatList :: FuncStatList
  , sLocker       :: L.Lock
  , sGrabQueue    :: GrabQueue
  , sJobQueue     :: JobQueue
  , sProcessJob   :: ProcessQueue
  , sAlive        :: TVar Bool
  , sChanList     :: TVar [Action]
  }

newtype SchedT m a = SchedT {unSchedT :: ReaderT SchedEnv m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , MonadReader SchedEnv
    , MonadThrow
    , MonadCatch
    , MonadMask
    )

deriving instance MonadBase IO m => MonadBase IO (SchedT m)

instance MonadTransControl SchedT where
  type StT SchedT a = StT (ReaderT SchedEnv) a
  liftWith = defaultLiftWith SchedT unSchedT
  restoreT = defaultRestoreT SchedT

instance MonadBaseControl IO m => MonadBaseControl IO (SchedT m) where
  type StM (SchedT m) a = ComposeSt SchedT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

type Task m = Async (StM (SchedT m) ())
type TaskList m = IOHashMap JobHandle (Task m)

runSchedT :: SchedEnv -> SchedT m a -> m a
runSchedT schedEnv = flip runReaderT schedEnv . unSchedT

initSchedEnv :: FilePath -> IO () -> IO SchedEnv
initSchedEnv sStorePath sCleanup = do
  createDirectoryIfMissing True sStorePath
  sFuncStatList <- newIOHashMap
  sLocker       <- L.new
  sGrabQueue    <- newGrabQueue
  sJobQueue     <- newIOHashMap
  sProcessJob   <- newIOHashMap
  sAlive        <- newTVarIO True
  sChanList     <- newTVarIO []
  sPollDelay    <- newTVarIO 300
  sRevertDelay  <- newTVarIO 300
  sTaskTimeout  <- newTVarIO 600
  pure SchedEnv{..}

startSchedT :: (MonadIO m, MonadBaseControl IO m) => SchedT m ()
startSchedT = do
  SchedEnv{..} <- ask
  runTask_ sRevertDelay revertProcessQueue
  runTask 300 $ saveJob "dump.db"
  taskList <- liftIO newIOHashMap
  runTask_ sPollDelay $ pollJob taskList
  runTask 0 $ runChanJob taskList
  mapM_ pushJob =<< loadJob "dump.db"
  mapM_ adjustFuncStat =<< liftIO (FL.keys sJobQueue)

  loadInt "poll-delay" sPollDelay
  loadInt "revert-delay" sPollDelay
  loadInt "task-timeout" sTaskTimeout

loadInt :: MonadIO m => FilePath -> TVar Int -> SchedT m ()
loadInt fn ref = do
  path <- (</> fn) <$> asks sStorePath
  liftIO $ do
    exists <- doesFileExist path
    when exists $ do
      v' <- readMaybe <$> readFile path
      case v' of
        Nothing -> pure ()
        Just v  -> atomically $ writeTVar ref v

saveInt :: MonadIO m => FilePath -> Int -> TVar Int -> SchedT m ()
saveInt fn v ref = do
  path <- (</> fn) <$> asks sStorePath
  liftIO $ do
    writeFile path $ show v
    atomically $ writeTVar ref v

setConfigInt :: MonadIO m => String -> Int -> SchedT m ()
setConfigInt key val = do
  SchedEnv {..} <- ask
  case key of
    "poll-delay"   -> saveInt "poll-delay" val sPollDelay
    "revert-delay" -> saveInt "revert-delay" val sRevertDelay
    "timeout"      -> saveInt "timeout" val sTaskTimeout
    _              -> pure ()

getConfigInt :: MonadIO m => String -> SchedT m Int
getConfigInt key = do
  SchedEnv {..} <- ask
  case key of
    "poll-delay"   -> liftIO $ readTVarIO sPollDelay
    "revert-delay" -> liftIO $ readTVarIO sRevertDelay
    "timeout"      -> liftIO $ readTVarIO sTaskTimeout
    _              -> pure 0


runTask :: (MonadIO m, MonadBaseControl IO m) => Int -> SchedT m () -> SchedT m ()
runTask d m = flip runTask_ m =<< liftIO (newTVarIO d)

runTask_ :: (MonadIO m, MonadBaseControl IO m) => TVar Int -> SchedT m () -> SchedT m ()
runTask_ d m = void . async $ do
  SchedEnv{..} <- ask
  void . runMaybeT . forever $ do
    delay <- liftIO $ readTVarIO d
    when (delay > 0) $ liftIO $ threadDelay $ delay * 1000 * 1000
    alive <- liftIO $ readTVarIO sAlive
    if alive then lift m
             else mzero

runChanJob :: (MonadIO m, MonadBaseControl IO m) => TaskList m -> SchedT m ()
runChanJob taskList = do
  cl <- asks sChanList
  al <- asks sAlive
  acts <- liftIO . atomically $ do
    acts <- readTVar cl
    if null acts then do
      st <- readTVar al
      if st then retry
            else pure []
    else do
      writeTVar cl []
      pure acts

  mapM_ (doChanJob taskList) acts

  where doChanJob :: (MonadIO m, MonadBaseControl IO m) => TaskList m -> Action -> SchedT m ()
        doChanJob tl (Add job) = reSchedJob tl job
        doChanJob tl (Remove job) = do
          w <- findTask tl job
          when (isJust w) $ do
            cancel (fromJust w)
            liftIO $ FL.delete tl (jHandle job)
        doChanJob tl Cancel = mapM_ cancel =<< liftIO (FL.elems tl)
        doChanJob tl PollJob = pollJob tl


pollDelay :: (MonadIO m, Num a) => SchedT m a
pollDelay = do
  d <- asks sPollDelay
  liftIO $ fromIntegral <$> readTVarIO d

pollJob :: (MonadIO m, MonadBaseControl IO m) => TaskList m -> SchedT m ()
pollJob taskList = do
  SchedEnv{..} <- ask
  mapM_ checkPoll =<< liftIO (FL.toList taskList)

  delay <- (+100) <$> pollDelay

  next <- liftIO $ (+ delay) <$> getEpochTime
  mapM_ (checkJob taskList) =<< liftIO (JQ.findLessJob sJobQueue next)

  where checkJob
          :: (MonadIO m, MonadBaseControl IO m)
          => TaskList m -> Job -> SchedT m ()
        checkJob tl job@Job{..} = do
          w <- findTask tl job
          delay <- pollDelay
          case w of
            Nothing -> do
              now <- liftIO getEpochTime
              when (jSchedAt > now || jSchedAt + delay < now) $ reSchedJob tl job
            Just w0 -> do
              r <- canRun jFuncName
              unless r $ cancel w0

        checkPoll
          :: (MonadIO m, MonadBaseControl IO m)
          => (JobHandle, Async (StM (SchedT m) ())) -> SchedT m ()
        checkPoll (jh, w) = do
          r <- poll w
          case r of
            Just (Right ()) -> liftIO $ FL.delete taskList jh
            _               -> pure ()

pushChanList :: MonadIO m => Action -> SchedT m ()
pushChanList act = do
  cl <- asks sChanList
  liftIO . atomically $ do
    l <- readTVar cl
    writeTVar cl (act:l)

pushJob :: MonadIO m => Job -> SchedT m ()
pushJob job@Job{..} = do
  SchedEnv{..} <- ask
  exists <- liftIO $ JQ.memberJob sJobQueue jFuncName jName
  isProc <- liftIO $ PQ.memberJob sProcessJob jFuncName (hashJobName jName)
  if exists then doPushJob
            else unless isProc doPushJob

  adjustFuncStat jFuncName

  where doPushJob :: MonadIO m => SchedT m ()
        doPushJob = do
          queue <- asks sJobQueue
          pushChanList (Add job)
          liftIO $ JQ.pushJob queue job

reSchedJob :: (MonadIO m, MonadBaseControl IO m) => TaskList m -> Job -> SchedT m ()
reSchedJob taskList job = do
  w <- findTask taskList job
  when (isJust w) $ do
    cancel (fromJust w)
    liftIO $ FL.delete taskList (jHandle job)

  delay <- (+100) <$> pollDelay
  next <- liftIO $ (+ delay) <$> getEpochTime
  when (jSchedAt job < next) $ do
    r <- canRun $ jFuncName job
    when r $ do
      w' <- schedJob taskList job
      liftIO $ FL.insert taskList (jHandle job) w'

findTask :: (MonadIO m) => TaskList m -> Job -> SchedT m (Maybe (Task m))
findTask taskList job = liftIO $ FL.lookup taskList (jHandle job)

canRun :: MonadIO m => FuncName -> SchedT m Bool
canRun fn = do
  stList <- asks sFuncStatList
  st0 <- liftIO $ FL.lookup stList fn
  case st0 of
    Nothing                  -> pure False
    Just FuncStat{sWorker=0} -> pure False
    Just _                   -> pure True

schedJob
  :: (MonadIO m, MonadBaseControl IO m)
  => TaskList m -> Job -> SchedT m (Task m)
schedJob taskList = async . schedJob_ taskList

schedJob_ :: MonadIO m => TaskList m -> Job -> SchedT m ()
schedJob_ taskList job@Job{..} = do
  SchedEnv{..} <- ask
  r <- canRun jFuncName
  when r $ do
    now <- liftIO getEpochTime
    when (jSchedAt > now) . liftIO . threadDelay . fromIntegral $ (jSchedAt - now) * 1000000
    FuncStat{..} <- liftIO . atomically $ do
      st <- FL.lookupSTM sFuncStatList jFuncName
      case st of
        Nothing                  -> retry
        Just FuncStat{sWorker=0} -> retry
        Just st'                 -> pure st'
    if sBroadcast then popAgentListThen taskList
                  else popAgentThen taskList

  where popAgentThen
          :: MonadIO m => TaskList m -> SchedT m ()
        popAgentThen tl = do
          SchedEnv{..} <- ask
          (jq, env0) <- liftIO $ atomically $ popAgentSTM sGrabQueue jFuncName
          alive <- liftIO $ runAgentT' env0 aAlive
          if alive then do
            liftIO $ do
              IL.insert jq (jHandle job)
              nextSchedAt <- getEpochTime
              PQ.insertJob sProcessJob job {jSchedAt = nextSchedAt}
            r <- doSubmitJob env0
            case r of
              Left _ -> do
                liftIO $ do
                  PQ.removeJob sProcessJob jFuncName (hashJobName jName)
                  IL.delete jq (jHandle job)
                schedJob_ tl job
              Right _ -> do
                adjustFuncStat jFuncName
                endSchedJob tl
          else schedJob_ tl job

        popAgentListThen :: MonadIO m => TaskList m -> SchedT m ()
        popAgentListThen tl = do
          SchedEnv{..} <- ask
          agents <- liftIO $ popAgentList sGrabQueue jFuncName
          mapM_ (doSubmitJob . snd) agents
          adjustFuncStat jFuncName
          unless (null agents) $ endSchedJob tl -- wait to resched the broadcast job

        doSubmitJob :: MonadIO m => AgentEnv' -> SchedT m (Either SomeException ())
        doSubmitJob agent = do
          SchedEnv{..} <- ask
          liftIO . try $ assignJob agent job

        endSchedJob :: MonadIO m => TaskList m -> SchedT m ()
        endSchedJob tl = do
          SchedEnv{..} <- ask
          liftIO $ do
            JQ.removeJob sJobQueue jFuncName jName
            FL.delete tl (jHandle job)

adjustFuncStat :: MonadIO m => FuncName -> SchedT m ()
adjustFuncStat fn = do
  SchedEnv{..} <- ask
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
  SchedEnv{..} <- ask
  liftIO $ do
    has <- JQ.memberJob sJobQueue (jFuncName job) (jName job)
    when has $ JQ.removeJob sJobQueue (jFuncName job) (jName job)

    isProc <- PQ.memberJob sProcessJob (jFuncName job) (hashJobName $ jName job)
    when isProc $ PQ.removeJob sProcessJob (jFuncName job) (hashJobName $ jName job)

  adjustFuncStat (jFuncName job)

  pushChanList (Remove job)

dumpJob :: MonadIO m => SchedT m [Job]
dumpJob = do
  js <- liftIO . JQ.dumpJob =<< asks sJobQueue
  js' <- liftIO . PQ.dumpJob =<< asks sProcessJob
  return $ js ++ js'

saveJob :: MonadIO m => FilePath -> SchedT m ()
saveJob fn = do
  path <- (</> fn) <$> asks sStorePath
  dumpJob >>= liftIO . encodeFile path

loadJob :: MonadIO m => FilePath -> SchedT m [Job]
loadJob fn = do
  path <- (</> fn) <$> asks sStorePath
  exists <- liftIO $ doesFileExist path
  if exists then liftIO $ decodeFile path
            else pure []

addFunc :: MonadIO m => FuncName -> SchedT m ()
addFunc n = broadcastFunc n False

broadcastFunc :: MonadIO m => FuncName -> Bool -> SchedT m ()
broadcastFunc n cast = do
  SchedEnv{..} <- ask
  liftIO . L.with sLocker $ FL.alter sFuncStatList updateStat n
  pushChanList PollJob

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just ((funcStat n) {sWorker = 1, sBroadcast = cast})
        updateStat (Just fs) = Just (fs { sWorker = sWorker fs + 1, sBroadcast = cast })

removeFunc :: MonadIO m => FuncName -> SchedT m ()
removeFunc n = do
  SchedEnv{..} <- ask
  liftIO . L.with sLocker $ FL.alter sFuncStatList updateStat n
  pushChanList PollJob

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just (funcStat n)
        updateStat (Just fs) = Just (fs { sWorker = max (sWorker fs - 1) 0 })

dropFunc :: MonadIO m => FuncName -> SchedT m ()
dropFunc n = do
  SchedEnv{..} <- ask
  liftIO . L.with sLocker $ do
    st <- FL.lookup sFuncStatList n
    when (isJust st) $
      when (sWorker (fromJust st) == 0) $ do
        FL.delete sFuncStatList n
        FL.delete sJobQueue n

  pushChanList PollJob

pushGrab :: MonadIO m => IOList FuncName -> IOList JobHandle -> AgentEnv' -> SchedT m ()
pushGrab funcList handleList ag = do
  queue <- asks sGrabQueue
  liftIO $ pushAgent queue funcList handleList ag

assignJob :: AgentEnv' -> Job -> IO ()
assignJob env0 job =
  runAgentT' env0 $ send (JobAssign (jHandle job) job)

failJob :: (MonadIO m, MonadBaseControl IO m) => JobHandle -> SchedT m ()
failJob jh = do
  SchedEnv{..} <- ask
  job <- liftIO $ PQ.lookupJob sProcessJob fn jn
  when (isJust job) $ do
    nextSchedAt <- liftIO getEpochTime
    retryJob ((fromJust job) {jSchedAt = nextSchedAt})

  where (fn, jn) = unHandle jh

retryJob :: MonadIO m => Job -> SchedT m ()
retryJob job = do
  SchedEnv{..} <- ask
  liftIO $ JQ.pushJob sJobQueue job
  liftIO $ PQ.removeJob sProcessJob fn (hashJobName jn)

  adjustFuncStat fn
  pushChanList (Add job)

  where  fn = jFuncName job
         jn = jName job


doneJob :: MonadIO m => JobHandle -> SchedT m ()
doneJob jh = do
  SchedEnv{..} <- ask
  job <- liftIO $ PQ.lookupJob sProcessJob fn jn
  when (isJust job) $ do
    liftIO $ PQ.removeJob sProcessJob fn jn
    adjustFuncStat fn

  where (fn, jn) = unHandle jh

schedLaterJob
  :: (MonadIO m, MonadBaseControl IO m)
  => JobHandle -> Int64 -> Int -> SchedT m ()
schedLaterJob jh later step = do
  SchedEnv{..} <- ask
  job <- liftIO $ PQ.lookupJob sProcessJob fn jn
  when (isJust job) $ do
    let job' = fromJust job

    nextSchedAt <- liftIO $ (+) later <$> getEpochTime
    retryJob job' {jSchedAt = nextSchedAt , jCount = jCount job' + step}

  where (fn, jn) = unHandle jh

status :: MonadIO m => SchedT m [FuncStat]
status = liftIO . FL.elems =<< asks sFuncStatList

revertProcessQueue :: (MonadIO m, MonadBaseControl IO m) => SchedT m ()
revertProcessQueue = do
  now <- liftIO getEpochTime
  queue <- asks sProcessJob
  tout <- liftIO . readTVarIO =<< asks sTaskTimeout
  mapM_ (failJob . jHandle)
    =<< filter (isTimeout now $ fromIntegral tout) <$> liftIO (PQ.dumpJob queue)
  where isTimeout :: Int64 -> Int64 -> Job -> Bool
        isTimeout t1 tout Job{jSchedAt = t} = (t + tout) < t1

shutdown :: (MonadIO m, MonadBaseControl IO m) => SchedT m ()
shutdown = do
  SchedEnv{..} <- ask
  pushChanList Cancel
  alive <- liftIO $ atomically $ do
    t <- readTVar sAlive
    writeTVar sAlive False
    return t
  when alive $ do
    saveJob "dump.db"
    void . async $ liftIO sCleanup
