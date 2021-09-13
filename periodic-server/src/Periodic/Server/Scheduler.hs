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

module Periodic.Server.Scheduler
  ( SchedT
  , runSchedT
  , SchedEnv
  , initSchedEnv
  , startSchedT
  , pushJob
  , pushGrab
  , failJob
  , doneJob
  , schedLaterJob
  , acquireLock
  , releaseLock
  , addFunc
  , removeFunc
  , broadcastFunc
  , dropFunc
  , removeJob
  , dumpJob
  , status
  , shutdown
  , keepalive
  , setConfigInt
  , getConfigInt
  , prepareWait
  , lookupPrevResult
  , waitResult
  , canRun
  ) where

import           Control.Monad              (forever, unless, void, when)
import           Control.Monad.Cont         (callCC, runContT)
import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans, lift)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.ByteString            (ByteString)
import           Data.Foldable              (forM_)
import           Data.IOMap                 (IOMap)
import qualified Data.IOMap                 as IOMap
import qualified Data.IOMap.STM             as IOMapS
import           Data.Int                   (Int64)
import qualified Data.List                  as L (delete)
import qualified Data.Map.Strict            as Map (filter, map)
import           Data.Maybe                 (fromJust, fromMaybe, isJust)
import           Data.UnixTime              (UnixDiffTime (..), UnixTime,
                                             diffUnixTime, getUnixTime)
import           Foreign.C.Types            (CTime (..))
import qualified Metro.Lock                 as L (Lock, new, with)
import           Metro.Utils                (getEpochTime)
import           Periodic.Server.FuncStat
import           Periodic.Server.GrabQueue
import           Periodic.Server.Hook       hiding (runHook)
import qualified Periodic.Server.Hook       as Hook
import           Periodic.Server.Persist    (Persist, State (..))
import qualified Periodic.Server.Persist    as P
import           Periodic.Types             (Msgid, Nid)
import           Periodic.Types.Internal    (LockName)
import           Periodic.Types.Job
import           System.Log.Logger          (debugM, errorM, infoM)
import           UnliftIO
import           UnliftIO.Concurrent        (threadDelay)

data Action = Add Job
    | Remove Job
    | Cancel
    | PollJob
    | Poll1 JobHandle

data WaitItem = WaitItem
    { itemTs    :: Int64
    , itemValue :: Maybe ByteString
    , itemWait  :: Int
    }

-- Cache runJob result
--                                   expiredAt, Nothing       retrySTM
--                                   expiredAt, Just bs       return bs
type WaitList = IOMap JobHandle WaitItem

data LockItem = LockItem JobHandle Int64
  deriving (Show)

instance Eq LockItem where
  LockItem jh0 _ == LockItem jh1 _ = jh0 == jh1

-- Distributed lock
--                                  acquired    locked
data LockInfo = LockInfo
    { acquired :: [LockItem]
    , locked   :: [LockItem]
    , maxCount :: Int
    }

type LockList = IOMap LockName LockInfo

batchScale :: Int
batchScale = 10

data SchedEnv db = SchedEnv
    { sPollInterval   :: TVar Int -- main poll loop every time interval
    -- revert process queue loop every time interval
    , sRevertInterval :: TVar Int -- revert process queue loop every time interval
    -- the task do timeout
    , sTaskTimeout    :: TVar Int -- the task do timeout
    -- lock timeout
    , sLockTimeout    :: TVar Int -- lock timeout
    -- max poll batch size
    , sMaxBatchSize   :: TVar Int -- max poll batch size
    -- client or worker keepalive
    , sKeepalive      :: TVar Int -- client or worker keepalive
    -- run job cache expiration
    , sExpiration     :: TVar Int -- run job cache expiration
    , sCleanup        :: IO ()
    , sFuncStatList   :: FuncStatList
    , sLocker         :: L.Lock
    , sGrabQueue      :: GrabQueue
    -- sched state, when false sched is exited.
    , sAlive          :: TVar Bool -- sched state, when false sched is exited.
    , sChanList       :: TVar [Action]
    , sWaitList       :: WaitList
    , sLockList       :: LockList
    , sPersist        :: db
    , sAssignJob      :: Nid -> Msgid -> Job -> IO Bool
    , sHook           :: Hook
    , sAssignJobTime  :: IOMap JobHandle UnixTime
    }

newtype SchedT db m a = SchedT {unSchedT :: ReaderT (SchedEnv db) m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , MonadReader (SchedEnv db)
    )

instance MonadUnliftIO m => MonadUnliftIO (SchedT db m) where
  withRunInIO inner = SchedT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runSchedT r)


type TaskList = IOMap JobHandle (Int64, Async ())

runSchedT :: SchedEnv db -> SchedT db m a -> m a
runSchedT schedEnv = flip runReaderT schedEnv . unSchedT

initSchedEnv
  :: (MonadUnliftIO m, Persist db)
  => P.PersistConfig db -> m ()
  -> (Nid -> Msgid -> Job -> IO Bool)
  -> Hook
  -> m (SchedEnv db)
initSchedEnv config sC sAssignJob sHook = do
  sFuncStatList   <- IOMap.empty
  sWaitList       <- IOMap.empty
  sLockList       <- IOMap.empty
  sLocker         <- L.new
  sGrabQueue      <- newGrabQueue
  sAlive          <- newTVarIO True
  sChanList       <- newTVarIO []
  sPollInterval   <- newTVarIO 300
  sRevertInterval <- newTVarIO 300
  sTaskTimeout    <- newTVarIO 600
  sLockTimeout    <- newTVarIO 300
  sMaxBatchSize   <- newTVarIO 250
  sKeepalive      <- newTVarIO 300
  sExpiration     <- newTVarIO 300
  sCleanup        <- toIO sC
  sPersist        <- liftIO $ P.newPersist config
  sAssignJobTime  <- IOMap.empty
  pure SchedEnv{..}

startSchedT :: (MonadUnliftIO m, Persist db) => SchedT db m ()
startSchedT = do
  liftIO $ infoM "Periodic.Server.Scheduler" "Scheduler started"
  SchedEnv{..} <- ask
  runTask_ sRevertInterval revertRunningQueue
  taskList <- IOMap.empty
  runTask_ sPollInterval $ pushChanList PollJob
  runTask 0 $ runChanJob taskList
  runTask 100 purgeExpired
  runTask 60 revertLockingQueue
  runTask 100 purgeEmptyLock

  loadInt "poll-interval" sPollInterval
  loadInt "revert-interval" sRevertInterval
  loadInt "timeout" sTaskTimeout
  loadInt "lock-timeout" sLockTimeout
  loadInt "keepalive" sKeepalive
  loadInt "max-batch-size" sMaxBatchSize
  loadInt "expiration" sExpiration

loadInt :: (MonadIO m, Persist db) => String -> TVar Int -> SchedT db m ()
loadInt name ref = do
  v <- liftIO . flip P.configGet name =<< asks sPersist
  case v of
    Nothing -> pure ()
    Just v' -> atomically $ writeTVar ref v'

saveInt :: (MonadIO m, Persist db) => String -> Int -> TVar Int -> SchedT db m ()
saveInt name v ref = do
  p <- asks sPersist
  liftIO $ P.configSet p name v
  atomically $ writeTVar ref v

setConfigInt :: (MonadIO m, Persist db) => String -> Int -> SchedT db m ()
setConfigInt key val = do
  SchedEnv {..} <- ask
  case key of
    "poll-interval"   -> saveInt "poll-interval" val sPollInterval
    "revert-interval" -> saveInt "revert-interval" val sRevertInterval
    "timeout"         -> saveInt "timeout" val sTaskTimeout
    "lock-timeout"    -> saveInt "lock-timeout" val sLockTimeout
    "keepalive"       -> saveInt "keepalive" val sKeepalive
    "max-batch-size"  -> saveInt "max-batch-size" val sMaxBatchSize
    "expiration"      -> saveInt "expiration" val sExpiration
    _                 -> pure ()

getConfigInt :: (MonadIO m, Persist db) => String -> SchedT db m Int
getConfigInt key = do
  SchedEnv {..} <- ask
  case key of
    "poll-interval"   -> readTVarIO sPollInterval
    "revert-interval" -> readTVarIO sRevertInterval
    "timeout"         -> readTVarIO sTaskTimeout
    "lock-timeout"    -> readTVarIO sLockTimeout
    "keepalive"       -> readTVarIO sKeepalive
    "max-batch-size"  -> readTVarIO sMaxBatchSize
    "expiration"      -> readTVarIO sExpiration
    _                 -> pure 0

keepalive :: Monad m => SchedT db m (TVar Int)
keepalive = asks sKeepalive

runTask :: (MonadUnliftIO m) => Int -> SchedT db m () -> SchedT db m ()
runTask d m = flip runTask_ m =<< newTVarIO d

runTask_ :: (MonadUnliftIO m) => TVar Int -> SchedT db m () -> SchedT db m ()
runTask_ d m = void . async $ do
  SchedEnv{..} <- ask
  (`runContT` pure) $ callCC $ \exit -> forever $ do
    interval <- readTVarIO d
    when (interval > 0) $ threadDelay $ interval * 1000 * 1000
    alive <- readTVarIO sAlive
    if alive then lift m
             else exit ()

runChanJob
  :: (MonadUnliftIO m, Persist db)
  => TaskList -> SchedT db m ()
runChanJob taskList = do
  cl <- asks sChanList
  al <- asks sAlive
  acts <- atomically $ do
    acts <- readTVar cl
    if null acts then do
      st <- readTVar al
      if st then retrySTM
            else pure []
    else do
      writeTVar cl []
      pure acts

  mapM_ doChanJob acts

  where doChanJob
          :: (MonadUnliftIO m, Persist db)
          => Action -> SchedT db m ()
        doChanJob (Add job)    = reSchedJob taskList job
        doChanJob (Remove job) = findTask taskList job >>= mapM_ (cancelWaitJob taskList (getHandle job))
        doChanJob Cancel       = mapM_ (cancel . snd) =<< IOMap.elems taskList
        doChanJob PollJob      = pollJob0 taskList
        doChanJob (Poll1 jh)   = pollJob1 taskList jh

cancelWaitJob :: MonadIO m => TaskList -> JobHandle -> Async () -> m ()
cancelWaitJob taskList jh w = do
  cancel w
  IOMap.delete jh taskList


pollJob0
  :: (MonadUnliftIO m, Persist db)
  => TaskList -> SchedT db m ()
pollJob0 taskList = do
  mapM_ checkPoll =<< IOMap.toList taskList
  funcList <- getAvaliableFuncList
  next <- getNextPoll
  pollJob_ taskList funcList next

  where checkPoll
          :: (MonadIO m)
          => (JobHandle, (Int64, Async ())) -> SchedT db m ()
        checkPoll (jh, (_, w)) = do
          r <- poll w
          case r of
            Just (Right ())  -> IOMap.delete jh taskList
            Just (Left e)  -> do
              IOMap.delete jh taskList
              case show e of
                "AsyncCancelled" -> pure ()
                ee -> liftIO $ errorM "Periodic.Server.Scheduler" ("Poll error: " ++ ee)
            Nothing -> do
              r0 <- canRun fn
              unless r0 $ cancelWaitJob taskList jh w

          where (fn, _) = unHandle jh

pollJob1
  :: (MonadUnliftIO m, Persist db)
  => TaskList -> JobHandle -> SchedT db m ()
pollJob1 taskList jh = do
  IOMap.delete jh taskList
  maxBatchSize <- readTVarIO =<< asks sMaxBatchSize
  size <- IOMap.size taskList
  when (size < maxBatchSize) $ do
    funcList <- getAvaliableFuncList
    next <- getNextPoll
    p <- asks sPersist
    count <- liftIO $ P.countPending p funcList next
    when (count > size) $ pollJob_ taskList funcList next

getNextPoll :: MonadIO m => SchedT db m Int64
getNextPoll = do
  now <- getEpochTime
  pollInterval <- asks sPollInterval
  (+ (100 + now)) . fromIntegral <$> readTVarIO pollInterval

getAvaliableFuncList :: MonadIO m => SchedT db m [FuncName]
getAvaliableFuncList = do
  stList <- asks sFuncStatList
  foldr foldFunc [] <$> IOMap.toList stList

  where foldFunc :: (FuncName, FuncStat) -> [FuncName] -> [FuncName]
        foldFunc (_, FuncStat{sWorker=0}) acc = acc
        foldFunc (fn, _) acc                  = fn:acc


pollJob_
  :: (MonadUnliftIO m, Persist db)
  => TaskList -> [FuncName] -> Int64 -> SchedT db m ()
pollJob_ _ [] _ = pure ()
pollJob_ taskList funcList next = do
  handles <- IOMap.keys taskList

  maxBatchSize <- readTVarIO =<< asks sMaxBatchSize
  p <- asks sPersist
  jobs <- liftIO $ P.getPendingJob p funcList next
            (maxBatchSize * batchScale + length handles)

  mapM_ (checkJob taskList) $ filter (flip notElem handles . getHandle) jobs

  where checkJob
          :: (MonadUnliftIO m, Persist db)
          => TaskList -> Job -> SchedT db m ()
        checkJob tl job = do
          w <- findTask tl job
          case w of
            Nothing -> do
              p <- asks sPersist
              isProc <- liftIO $ P.member p Running fn jn
              unless isProc $ reSchedJob tl job
            Just w0 -> do
              r <- canRun fn
              unless r $ cancelWaitJob taskList (getHandle job) w0

          where fn = getFuncName job
                jn = getName job

pushChanList :: MonadIO m => Action -> SchedT db m ()
pushChanList act = do
  cl <- asks sChanList
  atomically $ do
    l <- readTVar cl
    writeTVar cl (act:l)

pushJob :: (MonadIO m, Persist db) => Job -> SchedT db m ()
pushJob job = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("pushJob: " ++ show (getHandle job))
  t0 <- liftIO getUnixTime
  p <- asks sPersist
  isRunning <- liftIO $ P.member p Running fn jn
  unless isRunning $ do
    job' <- fixedSchedAt job
    liftIO $ P.insert p Pending fn jn job'
    pushChanList (Add job')

  getDuration t0 >>= runHook eventPushJob job

  where fn = getFuncName job
        jn = getName job


fixedSchedAt :: MonadIO m => Job -> SchedT db m Job
fixedSchedAt job = do
  now <- getEpochTime
  if getSchedAt job < now then
    return $ setSchedAt now job
  else return job

reSchedJob :: (MonadUnliftIO m, Persist db) => TaskList -> Job -> SchedT db m ()
reSchedJob taskList job = do
  w <- findTask taskList job
  forM_ w (cancelWaitJob taskList (getHandle job))

  next <- getNextPoll
  when (getSchedAt job < next) $ do
    r <- canRun $ getFuncName job
    c <- check taskList
    when (r && c) $ do
      w' <- schedJob job
      IOMap.insert (getHandle job) (getSchedAt job, w') taskList
  where check :: (MonadIO m) => TaskList -> SchedT db m Bool
        check tl = do
          maxBatchSize <- readTVarIO =<< asks sMaxBatchSize
          size <- IOMap.size tl
          if size < maxBatchSize * batchScale then return True
          else do
            lastTask <- findLastTask tl
            case lastTask of
              Nothing -> return True
              Just (sc, jh, w) ->
                if sc < getSchedAt job then return False
                else do
                  cancelWaitJob taskList jh w
                  return True

findTask :: MonadIO m => TaskList -> Job -> SchedT db m (Maybe (Async ()))
findTask taskList job = fmap snd <$> IOMap.lookup (getHandle job) taskList

findLastTask :: MonadIO m => TaskList -> SchedT db m (Maybe (Int64, JobHandle, Async ()))
findLastTask = IOMap.foldrWithKey f Nothing
  where f :: JobHandle
          -> (Int64, a)
          -> Maybe (Int64, JobHandle, a)
          -> Maybe (Int64, JobHandle, a)
        f jh (sc, t) Nothing = Just (sc, jh, t)
        f jh (sc, t) (Just (sc1, jh1, t1))
          | sc > sc1 = Just (sc, jh, t)
          | otherwise = Just (sc1, jh1, t1)

canRun :: MonadIO m => FuncName -> SchedT db m Bool
canRun fn = asks sFuncStatList >>= flip canRun_ fn

canRun_ :: MonadIO m => FuncStatList -> FuncName -> m Bool
canRun_ stList fn = do
  st0 <- IOMap.lookup fn stList
  case st0 of
    Nothing                  -> pure False
    Just FuncStat{sWorker=0} -> pure False
    Just _                   -> pure True

schedJob
  :: (MonadUnliftIO m, Persist db)
  => Job -> SchedT db m (Async ())
schedJob = async . schedJob_

schedJob_ :: (MonadUnliftIO m, Persist db) => Job -> SchedT db m ()
schedJob_ job = do
  SchedEnv{..} <- ask
  r <- canRun fn
  when r $ do
    now <- getEpochTime
    when (schedAt > now + 1) . threadDelay . fromIntegral $ (schedAt - now) * 1000000
    FuncStat{..} <- atomically $ do
      st <- IOMapS.lookup fn sFuncStatList
      case st of
        Nothing                  -> retrySTM
        Just FuncStat{sWorker=0} -> retrySTM
        Just st'                 -> pure st'
    if sBroadcast then popAgentListThen
                  else popAgentThen

  where fn = getFuncName job
        jn = getName job
        schedAt = getSchedAt job
        jh = getHandle job

        popAgentThen
          :: (MonadUnliftIO m, Persist db) => SchedT db m ()
        popAgentThen = do
          SchedEnv{..} <- ask
          (nid, msgid) <- liftIO $ popAgent sGrabQueue fn
          liftIO $ P.insert sPersist Running fn jn job
          t <- liftIO getUnixTime
          IOMap.insert (getHandle job) t sAssignJobTime
          r <- liftIO $ sAssignJob nid msgid job
          if r then endSchedJob
               else do
                 liftIO $ P.insert sPersist Pending fn jn job
                 schedJob_ job

        popAgentListThen :: (MonadUnliftIO m) => SchedT db m ()
        popAgentListThen = do
          SchedEnv{..} <- ask
          agents <- popAgentList sGrabQueue fn
          liftIO $ mapM_ (\(nid, msgid) -> sAssignJob nid msgid job) agents
          unless (null agents) endSchedJob -- wait to resched the broadcast job

        endSchedJob :: MonadIO m => SchedT db m ()
        endSchedJob = pushChanList (Poll1 jh)

adjustFuncStat :: (MonadIO m, Persist db) => FuncName -> SchedT db m ()
adjustFuncStat fn = do
  SchedEnv{..} <- ask
  size <- liftIO $ P.size sPersist Pending fn
  sizePQ <- liftIO $ P.size sPersist Running fn
  sizeL <- liftIO $ P.size sPersist Locking fn
  sc <- liftIO $ P.minSchedAt sPersist fn

  schedAt <- if sc > 0 then pure sc else getEpochTime

  IOMap.alter (update (size + sizePQ + sizeL) sizePQ sizeL schedAt) fn sFuncStatList

  where update :: Int64 -> Int64 -> Int64 -> Int64 -> Maybe FuncStat -> Maybe FuncStat
        update size sizePQ sizeL schedAt st =
          Just ((fromMaybe (funcStat fn) st) { sJob = size
                                             , sRunning = sizePQ
                                             , sLocking = sizeL
                                             , sSchedAt = schedAt
                                             })

removeJob :: (MonadIO m, Persist db) => Job -> SchedT db m ()
removeJob job = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("removeJob: " ++ show (getHandle job))
  t0 <- liftIO getUnixTime
  p <- asks sPersist
  liftIO $ P.delete p fn jn

  pushChanList (Remove job)
  pushResult jh ""
  getDuration t0 >>= runHook eventRemoveJob job

  where jn = getName job
        fn = getFuncName job
        jh = getHandle job

dumpJob :: (MonadIO m, Persist db) => SchedT db m [Job]
dumpJob = liftIO . P.dumpJob =<< asks sPersist

alterFunc :: (MonadIO m, Persist db) => FuncName -> (Maybe FuncStat -> Maybe FuncStat) -> SchedT db m ()
alterFunc n f = do
  SchedEnv{..} <- ask
  IOMap.alter f n sFuncStatList
  liftIO $ P.insertFuncName sPersist n
  pushChanList PollJob

addFunc :: (MonadIO m, Persist db) => FuncName -> SchedT db m ()
addFunc n = broadcastFunc n False

broadcastFunc :: (MonadIO m, Persist db) => FuncName -> Bool -> SchedT db m ()
broadcastFunc n cast = do
  liftIO $ debugM "Periodic.Server.Scheduler" (h ++ ": " ++ show n)
  alterFunc n updateStat

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just ((funcStat n) {sWorker = 1, sBroadcast = cast})
        updateStat (Just fs) = Just (fs { sWorker = sWorker fs + 1, sBroadcast = cast })

        h = if cast then "broadcastFunc" else "addFunc"

removeFunc :: (MonadIO m, Persist db) => FuncName -> SchedT db m ()
removeFunc n = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("removeFunc: " ++ show n)
  alterFunc n updateStat

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just (funcStat n)
        updateStat (Just fs) = Just (fs { sWorker = max (sWorker fs - 1) 0 })

dropFunc :: (MonadUnliftIO m, Persist db) => FuncName -> SchedT db m ()
dropFunc n = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("dropFunc: " ++ show n)
  SchedEnv{..} <- ask
  L.with sLocker $ do
    st <- IOMap.lookup n sFuncStatList
    case st of
      Just FuncStat{sWorker=0} -> do
        IOMap.delete n sFuncStatList
        liftIO $ P.removeFuncName sPersist n
      _                        -> pure ()

  pushChanList PollJob

pushGrab :: MonadIO m => TVar [FuncName] -> Nid -> Msgid -> SchedT db m ()
pushGrab funcList nid msgid = do
  t0 <- liftIO getUnixTime
  queue <- asks sGrabQueue
  pushAgent queue funcList nid msgid
  getDuration t0 >>= runHook eventPushGrab (FuncName "pushgrab")

failJob :: (MonadUnliftIO m, Persist db) => JobHandle -> SchedT db m ()
failJob jh = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("failJob: " ++ show jh)
  dura <- getJobDuration jh
  runHook eventFailJob jh dura
  releaseLock' jh
  isWaiting <- existsWaitList jh
  if isWaiting then do
    removeFromWaitList jh
    doneJob jh ""
  else do
    p <- asks sPersist
    job <- liftIO $ P.lookup p Running fn jn
    when (isJust job) $ do
      nextSchedAt <- getEpochTime
      retryJob $ setSchedAt nextSchedAt $ fromJust job

  where (fn, jn) = unHandle jh

retryJob :: (MonadIO m, Persist db) => Job -> SchedT db m ()
retryJob job = do
  p <- asks sPersist
  liftIO $ P.insert p Pending fn jn job

  pushChanList (Add job)

  where  fn = getFuncName job
         jn = getName job

getJobDuration
  :: (MonadIO m, Persist db)
  => JobHandle -> SchedT db m Double
getJobDuration jh = do
  h <- asks sAssignJobTime
  m <- IOMap.lookup jh h
  IOMap.delete jh h
  case m of
    Nothing -> pure 0
    Just t0 -> getDuration t0

getDuration :: MonadIO m => UnixTime -> SchedT db m Double
getDuration t0 = do
  t1 <- liftIO getUnixTime
  case t1 `diffUnixTime` t0 of
    UnixDiffTime (CTime s) u -> pure $ fromIntegral s + fromIntegral u / 1000000


doneJob
  :: (MonadUnliftIO m, Persist db)
  => JobHandle -> ByteString -> SchedT db m ()
doneJob jh w = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("doneJob: " ++ show jh)
  p <- asks sPersist
  getJobDuration jh >>= runHook eventDoneJob jh
  releaseLock' jh
  liftIO $ P.delete p fn jn
  pushResult jh w
  where (fn, jn) = unHandle jh

schedLaterJob
  :: (MonadUnliftIO m, Persist db)
  => JobHandle -> Int64 -> Int -> SchedT db m ()
schedLaterJob jh later step = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("schedLaterJob: " ++ show jh)
  releaseLock' jh
  isWaiting <- existsWaitList jh
  if isWaiting then do
    removeFromWaitList jh
    doneJob jh ""
  else do
    p <- asks sPersist
    job <- liftIO $ P.lookup p Running fn jn
    when (isJust job) $ do
      let job' = fromJust job

      nextSchedAt <- (later +) <$> getEpochTime

      getJobDuration jh >>= runHook eventSchedLaterJob jh
      retryJob $ setCount (getCount job' + step) $ setSchedAt nextSchedAt job'

  where (fn, jn) = unHandle jh

acquireLock
  :: (MonadUnliftIO m, Persist db)
  => LockName -> Int -> JobHandle -> SchedT db m Bool
acquireLock name count jh = do
  t0 <- liftIO getUnixTime
  r <- acquireLock_ name count jh
  getDuration t0 >>= runHook eventAcquireLock name
  return r

acquireLock_
  :: (MonadUnliftIO m, Persist db)
  => LockName -> Int -> JobHandle -> SchedT db m Bool
acquireLock_ name count jh = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("acquireLock: " ++ show name ++ " " ++ show count ++ " " ++ show jh)
  locker <- asks sLocker
  L.with locker $ do
    lockList <- asks sLockList
    p <- asks sPersist
    j <- liftIO $ P.lookup p Running fn jn
    case j of
      Nothing -> pure True
      Just job -> do
        now <- getEpochTime
        let item = LockItem jh now
        r <- atomically $ do
          l <- IOMapS.lookup name lockList
          case l of
            Nothing -> do
              IOMapS.insert name LockInfo
                { acquired = [item]
                , locked = []
                , maxCount = count
                } lockList
              pure True
            Just info@LockInfo {..} -> do
              let newCount = max maxCount count
              if item `elem` acquired then pure True
              else if item `elem` locked then pure False
              else
                if length acquired < maxCount then do
                  IOMapS.insert name info
                    { acquired = acquired ++ [item]
                    , maxCount = newCount
                    } lockList
                  pure True
                else do
                  IOMapS.insert name info
                    { locked = locked ++ [item]
                    , maxCount = newCount
                    } lockList
                  pure False

        unless r $ liftIO $ P.insert p Locking fn jn job
        return r

  where (fn, jn) = unHandle jh

releaseLock
  :: (MonadUnliftIO m, Persist db)
  => LockName -> JobHandle -> SchedT db m ()
releaseLock name jh = do
  t0 <- liftIO getUnixTime
  locker <- asks sLocker
  L.with locker $ releaseLock_ name jh
  getDuration t0 >>= runHook eventReleaseLock name

releaseLock_
  :: (MonadUnliftIO m, Persist db)
  => LockName -> JobHandle -> SchedT db m ()
releaseLock_ name jh = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("releaseLock: " ++ show name ++ " " ++ show jh)
  p <- asks sPersist
  lockList <- asks sLockList
  h <- atomically $ do
    l <- IOMapS.lookup name lockList
    case l of
      Nothing -> pure Nothing
      Just info@LockInfo {..} ->
        if item `elem` acquired then
          case locked of
            [] -> do
              IOMapS.insert name info
                { acquired = L.delete item acquired
                } lockList
              pure Nothing
            x:xs -> do
              IOMapS.insert name info
                { acquired = L.delete item acquired
                , locked   = xs
                } lockList
              pure $ Just x
        else pure Nothing

  case h of
    Nothing -> pure ()
    Just (LockItem hh _) -> do
      let (fn, jn) = unHandle hh
      j <- liftIO $ P.lookup p Locking fn jn
      case j of
        Nothing  -> releaseLock_ name hh
        Just job -> do
          liftIO $ P.insert p Pending fn jn job
          pushChanList (Add job)

  where item = LockItem jh 0

releaseLock'
  :: (MonadUnliftIO m, Persist db)
  => JobHandle -> SchedT db m ()
releaseLock' jh = do
  lockList <- asks sLockList
  names <- IOMap.foldrWithKey foldFunc [] lockList
  mapM_ (`releaseLock` jh) names

  where foldFunc :: LockName -> LockInfo -> [LockName] -> [LockName]
        foldFunc n LockInfo {..} acc
          | item `elem` acquired = n : acc
          | item `elem` locked   = n : acc
          | otherwise          = acc

        item = LockItem jh 0

purgeEmptyLock :: MonadIO m => SchedT db m ()
purgeEmptyLock = do
  lockList <- asks sLockList
  tout <- fmap fromIntegral . readTVarIO =<< asks sLockTimeout
  now <- getEpochTime
  IOMap.modifyIOMap (Map.filter filterFunc . Map.map (mapFunc (now - tout))) lockList

  where filterFunc :: LockInfo -> Bool
        filterFunc LockInfo {..}
          | null acquired && null locked = False
          | otherwise = True

        mapFunc :: Int64 -> LockInfo -> LockInfo
        mapFunc e i = i { locked = filter ff (locked i), acquired = filter ff (acquired i) }
          where ff :: LockItem -> Bool
                ff (LockItem _ t) = t > e

getMaxLockCount :: MonadUnliftIO m => Int -> SchedT db m Int
getMaxLockCount minV = do
  lockList <- asks sLockList
  maximum . (minV:) . map maxCount <$> IOMap.elems lockList


status :: (MonadIO m, Persist db) => SchedT db m [FuncStat]
status = do
  mapM_ adjustFuncStat =<< liftIO . P.funcList =<< asks sPersist
  IOMap.elems =<< asks sFuncStatList

revertRunningQueue :: (MonadUnliftIO m, Persist db) => SchedT db m ()
revertRunningQueue = do
  now <- getEpochTime
  tout <- fmap fromIntegral . readTVarIO =<< asks sTaskTimeout
  p <- asks sPersist
  handles <- liftIO $ filter (check now tout) <$> P.getRunningJob p (now - tout)
  mapM_ (failJob . getHandle) handles

  where check :: Int64 -> Int64 -> Job -> Bool
        check now t0 job
          | getTimeout job > 0 = getSchedAt job + fromIntegral (getTimeout job) < now
          | otherwise = getSchedAt job + fromIntegral t0 < now

revertLockingQueue :: (MonadUnliftIO m, Persist db) => SchedT db m ()
revertLockingQueue = mapM_ checkAndReleaseLock =<< liftIO . P.funcList =<< asks sPersist

  where checkAndReleaseLock
          :: (MonadUnliftIO m, Persist db)
          => FuncName -> SchedT db m ()
        checkAndReleaseLock fn = do
          p <- asks sPersist
          count <- getMaxLockCount 10
          handles <- liftIO $ P.getLockedJob p fn count
          mapM_ pushJob handles


purgeExpired :: MonadIO m => SchedT db m ()
purgeExpired = do
  now <- getEpochTime
  wl <- asks sWaitList
  ex <- fmap fromIntegral . readTVarIO =<< asks sExpiration
  atomically $ do
    ks <- IOMapS.foldrWithKey (foldFunc (check (now - ex))) [] wl
    mapM_ (`IOMapS.delete` wl) ks

  where foldFunc :: (WaitItem -> Bool) -> JobHandle -> WaitItem -> [JobHandle] -> [JobHandle]
        foldFunc f jh v acc | f v = jh : acc
                            | otherwise = acc

        check :: Int64 -> WaitItem -> Bool
        check t0 item = itemTs item < t0

shutdown :: (MonadUnliftIO m) => SchedT db m ()
shutdown = do
  liftIO $ infoM "Periodic.Server.Scheduler" "Scheduler shutdown"
  SchedEnv{..} <- ask
  pushChanList Cancel
  alive <- atomically $ do
    t <- readTVar sAlive
    writeTVar sAlive False
    return t
  when alive . void . async $ liftIO sCleanup

prepareWait :: MonadIO m => Job -> SchedT db m ()
prepareWait job = pushResult_ updateWL jh
  where updateWL :: Int64 -> Maybe WaitItem -> Maybe WaitItem
        updateWL now Nothing       = Just $ WaitItem {itemTs = now, itemValue = Nothing, itemWait = 1}
        updateWL now (Just item) = Just $ item {itemTs = now, itemWait = itemWait item + 1}

        jh = getHandle job

waitResult :: MonadIO m => TVar Bool -> Job -> SchedT db m ByteString
waitResult state job = do
  wl <- asks sWaitList
  atomically $ do
    st <- readTVar state
    if st then do
      w0 <- IOMapS.lookup jh wl
      case w0 of
        Nothing   -> pure ""
        Just item ->
          case itemValue item of
            Nothing -> retrySTM
            Just w1 -> do

              if itemWait item > 1 then
                IOMapS.insert jh item { itemWait = itemWait item - 1 } wl
              else
                IOMapS.delete jh wl

              pure w1

     else pure ""

  where jh = getHandle job

pushResult
  :: MonadIO m
  => JobHandle -> ByteString -> SchedT db m ()
pushResult jh w = pushResult_ updateWL jh
  where updateWL :: Int64 -> Maybe WaitItem -> Maybe WaitItem
        updateWL _ Nothing       = Nothing
        updateWL now (Just item) = Just item {itemTs=now, itemValue = Just w}

pushResult_
  :: MonadIO m
  => (Int64 -> Maybe WaitItem -> Maybe WaitItem)
  -> JobHandle -> SchedT db m ()
pushResult_ f jh = do
  wl <- asks sWaitList
  now <- getEpochTime
  IOMap.alter (f now) jh wl

existsWaitList :: MonadIO m => JobHandle -> SchedT db m Bool
existsWaitList jh = do
  wl <- asks sWaitList
  isJust <$> IOMap.lookup jh wl

lookupPrevResult :: MonadIO m => Job -> SchedT db m (Maybe ByteString)
lookupPrevResult job = do
  wl <- asks sWaitList
  r <- IOMap.lookup jh wl
  case r of
    Nothing                               -> pure Nothing
    (Just WaitItem {itemValue = Nothing}) -> pure Nothing
    (Just WaitItem {itemValue = Just v})  -> pure (Just v)

  where jh = getHandle job

removeFromWaitList :: MonadIO m => JobHandle -> SchedT db m ()
removeFromWaitList jh = do
  wl <- asks sWaitList
  IOMap.delete jh wl

runHook :: (MonadIO m, GetHookName a) => HookEvent -> a -> Double -> SchedT db m ()
runHook evt n c = do
  hook <- asks sHook
  Hook.runHook hook evt n c
