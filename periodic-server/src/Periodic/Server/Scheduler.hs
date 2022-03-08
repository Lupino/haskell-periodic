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
  , runJob
  , pushJob
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
  , canRun
  ) where

import           Control.Monad              (forever, unless, void, when)
import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.ByteString            (ByteString)
import           Data.Foldable              (forM_)
import           Data.IOMap                 (IOMap)
import qualified Data.IOMap                 as IOMap
import qualified Data.IOMap.STM             as IOMapS
import           Data.Int                   (Int64)
import qualified Data.List                  as L (delete)
import qualified Data.Map.Strict            as Map (filter, map)
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.UnixTime              (UnixDiffTime (..), UnixTime,
                                             diffUnixTime, getUnixTime)
import           Foreign.C.Types            (CTime (..))
import qualified Metro.Lock                 as L (Lock, new, with)
import           Metro.SessionPool          (PoolSize (..))
import           Metro.Utils                (getEpochTime)
import           Periodic.Server.FuncStat
import           Periodic.Server.GrabQueue
import           Periodic.Server.Hook       hiding (runHook)
import qualified Periodic.Server.Hook       as Hook
import           Periodic.Server.Persist    (Persist, State (..))
import qualified Periodic.Server.Persist    as P
import           Periodic.Server.SchedPool  (SchedPool, newSchedPool,
                                             runSchedPool)
import qualified Periodic.Server.SchedPool  as Pool
import           Periodic.Types             (Msgid, Nid)
import           Periodic.Types.Internal    (LockName)
import           Periodic.Types.Job
import           System.Log.Logger          (debugM, infoM)
import           UnliftIO                   hiding (poll)
import           UnliftIO.Concurrent        (threadDelay)

type Waiter = (Nid, Msgid)

data WaitItem = WaitItem
    { itemTs      :: Int64
    , itemWaiters :: [Waiter]
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


data SchedEnv db = SchedEnv
    -- the task do timeout
    { sTaskTimeout   :: TVar Int
    -- lock timeout
    , sLockTimeout   :: TVar Int
    -- max poll batch size
    , sMaxBatchSize  :: TVar Int
    -- client or worker keepalive
    , sKeepalive     :: TVar Int
    -- run job cache expiration
    , sExpiration    :: TVar Int
    , sCleanup       :: IO ()
    , sFuncStatList  :: FuncStatList
    , sLocker        :: L.Lock
    , sGrabQueue     :: GrabQueue
    -- sched state, when false sched is exited.
    , sPollJob       :: TMVar ()
    , sChanList      :: TQueue Job
    , sWaitList      :: WaitList
    , sLockList      :: LockList
    , sPersist       :: db
    , sAssignJob     :: Nid -> Msgid -> Job -> IO Bool
    , sPushData      :: Nid -> Msgid -> ByteString -> IO ()
    , sHook          :: Hook
    , sAssignJobTime :: IOMap JobHandle UnixTime
    , sMaxPoolSize   :: TVar Int
    , sSchedPoolList :: IOMap FuncName SchedPool
    , sPushList      :: TQueue Job
    , sTaskList      :: TVar [Async ()]
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


runSchedT :: SchedEnv db -> SchedT db m a -> m a
runSchedT schedEnv = flip runReaderT schedEnv . unSchedT

initSchedEnv
  :: (MonadUnliftIO m, Persist db)
  => P.PersistConfig db
  -> GrabQueue
  -> m ()
  -> (Nid -> Msgid -> Job -> IO Bool)
  -> (Nid -> Msgid -> ByteString -> IO ())
  -> Hook
  -> PoolSize
  -> m (SchedEnv db)
initSchedEnv config sGrabQueue sC sAssignJob sPushData sHook (PoolSize sMaxPoolSize) = do
  sFuncStatList   <- IOMap.empty
  sWaitList       <- IOMap.empty
  sLockList       <- IOMap.empty
  sLocker         <- L.new
  sPollJob        <- newEmptyTMVarIO
  sChanList       <- newTQueueIO
  sTaskTimeout    <- newTVarIO 600
  sLockTimeout    <- newTVarIO 300
  sMaxBatchSize   <- newTVarIO 500
  sKeepalive      <- newTVarIO 300
  sExpiration     <- newTVarIO 300
  sCleanup        <- toIO sC
  sPersist        <- liftIO $ P.newPersist config
  sAssignJobTime  <- IOMap.empty
  sSchedPoolList  <- IOMap.empty

  sPushList <- newTQueueIO
  sTaskList <- newTVarIO []
  pure SchedEnv{..}

startSchedT :: (MonadUnliftIO m, Persist db) => SchedT db m ()
startSchedT = do
  liftIO $ infoM "Periodic.Server.Scheduler" "Scheduler started"
  SchedEnv{..} <- ask
  runTask 100 revertRunningQueue
  runTask 1   runPollJob
  runTask 0   runChanJob
  runTask 0   runPushJob
  runTask 100 purgeExpired
  runTask 60  revertLockedQueue
  runTask 60  pushPollJob
  runTask 100 purgeEmptyLock

  loadInt "timeout" sTaskTimeout
  loadInt "lock-timeout" sLockTimeout
  loadInt "keepalive" sKeepalive
  loadInt "max-batch-size" sMaxBatchSize
  loadInt "max-pool-size" sMaxPoolSize
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
    "timeout"        -> saveInt "timeout" val sTaskTimeout
    "lock-timeout"   -> saveInt "lock-timeout" val sLockTimeout
    "keepalive"      -> saveInt "keepalive" val sKeepalive
    "max-batch-size" -> saveInt "max-batch-size" val sMaxBatchSize
    "max-pool-size"  -> saveInt "max-pool-size" val sMaxPoolSize
    "expiration"     -> saveInt "expiration" val sExpiration
    _                -> pure ()

getConfigInt :: (MonadIO m, Persist db) => String -> SchedT db m Int
getConfigInt key = do
  SchedEnv {..} <- ask
  case key of
    "timeout"        -> readTVarIO sTaskTimeout
    "lock-timeout"   -> readTVarIO sLockTimeout
    "keepalive"      -> readTVarIO sKeepalive
    "max-batch-size" -> readTVarIO sMaxBatchSize
    "max-pool-size"  -> readTVarIO sMaxPoolSize
    "expiration"     -> readTVarIO sExpiration
    _                -> pure 0

keepalive :: Monad m => SchedT db m (TVar Int)
keepalive = asks sKeepalive

runTask :: (MonadUnliftIO m) => Int -> SchedT db m () -> SchedT db m ()
runTask delay m = do
  io <- async $ forever $ do
    when (delayUS > 0) $ threadDelay delayUS
    m

  taskList <- asks sTaskList
  atomically $ modifyTVar' taskList (io:)

  where delayUS = delay * 1000 * 1000


runPollJob :: (MonadUnliftIO m, Persist db) => SchedT db m ()
runPollJob = do
  cl <- asks sPollJob
  atomically $ takeTMVar cl
  pollJob

pollJob :: (MonadIO m, Persist db) => SchedT db m ()
pollJob = do
  funcList <- getAvaliableFuncList
  next <- getNextPoll
  poolList <- asks sSchedPoolList
  forM_ funcList $ \fn -> do
    mPool <- IOMap.lookup fn poolList
    case mPool of
      Nothing -> pure ()
      Just pool -> do
        handles <- Pool.getHandleList pool
        let size = length handles
        p <- asks sPersist
        count <- liftIO $ P.countPending p [fn] next
        maxBatchSize <- readTVarIO =<< asks sMaxBatchSize
        when (count > size && maxBatchSize > size) $
          liftIO (P.getPendingJob p [fn] next (maxBatchSize + size))
           >>= mapM_ pushChanJob . filter (flip notElem handles . getHandle)


getNextPoll :: MonadIO m => m Int64
getNextPoll = (+100) <$> getEpochTime

getAvaliableFuncList :: MonadIO m => SchedT db m [FuncName]
getAvaliableFuncList = do
  stList <- asks sFuncStatList
  foldr foldFunc [] <$> IOMap.toList stList

  where foldFunc :: (FuncName, FuncStat) -> [FuncName] -> [FuncName]
        foldFunc (_, FuncStat{sWorker=0}) acc = acc
        foldFunc (fn, _) acc                  = fn:acc


pushPollJob :: MonadIO m => SchedT db m ()
pushPollJob = do
  cl <- asks sPollJob
  void $ atomically $ tryPutTMVar cl ()


pushChanJob :: MonadIO m => Job -> SchedT db m ()
pushChanJob job = do
  cl <- asks sChanList
  atomically $ writeTQueue cl job

runChanJob :: (MonadUnliftIO m, Persist db) => SchedT db m ()
runChanJob = do
  cl <- asks sChanList
  job <- atomically $ readTQueue cl
  reSchedJob job


runJob :: (MonadIO m, Persist db) => Job -> SchedT db m ()
runJob job = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("runJob: " ++ show (getHandle job))
  SchedEnv{..} <- ask
  tout <- fromIntegral <$> readTVarIO sTaskTimeout
  delay <- registerDelay (getJobTimeout tout job)
  mAgent <- atomically $ do
    mAgent <- popAgentSTM sGrabQueue fn
    case mAgent of
      Nothing    -> do
        isTimeout <- readTVar delay
        if isTimeout then pure Nothing
                     else retrySTM
      _ -> pure mAgent

  liftIO $ P.insert sPersist Running job
  t <- liftIO getUnixTime
  IOMap.insert (getHandle job) t sAssignJobTime

  case mAgent of
    Nothing -> doneJob_ (getHandle job) "failed"
    Just (nid, msgid) -> do
      r <- liftIO $ sAssignJob nid msgid job
      if r then pure ()
           else doneJob_ (getHandle job) "failed"

  where fn = getFuncName job


pushJob :: MonadIO m => Job -> SchedT db m ()
pushJob job = do
  pl <- asks sPushList
  atomically $ writeTQueue pl job


runPushJob :: (MonadIO m, Persist db) => SchedT db m ()
runPushJob = do
  pl <- asks sPushList
  job <- atomically $ readTQueue pl
  let fn = getFuncName job
      jn = getName job

  liftIO $ debugM "Periodic.Server.Scheduler" ("pushJob: " ++ show (getHandle job))
  t0 <- liftIO getUnixTime
  p <- asks sPersist
  isRunning <- liftIO $ isJust <$> P.getOne p Running fn jn
  unless isRunning $ do
    job' <- fixedSchedAt job
    liftIO $ P.insert p Pending job'
    pushChanJob job'

  getDuration t0 >>= runHook eventPushJob job


fixedSchedAt :: MonadIO m => Job -> SchedT db m Job
fixedSchedAt job = do
  now <- getEpochTime
  if getSchedAt job < now then
    return $ setSchedAt now job
  else return job

reSchedJob :: (MonadUnliftIO m, Persist db) => Job -> SchedT db m ()
reSchedJob job = do
  next <- getNextPoll
  when (getSchedAt job < next) $ do
    r <- canRun $ getFuncName job
    when r $ do
      SchedEnv{..} <- ask
      mPool <- IOMap.lookup fn sSchedPoolList
      pool <- case mPool of
        Nothing -> do
          pool <- newSchedPool sMaxPoolSize sMaxBatchSize
            (void $ tryPutTMVar sPollJob ()) $ \fn1 -> do
            mFuncStat <- IOMapS.lookup fn1 sFuncStatList
            case mFuncStat of
              Nothing                  -> pure []
              Just FuncStat{sWorker=0} -> pure []
              Just st -> do
                if sBroadcast st then popAgentListSTM sGrabQueue fn1
                else do
                  mAgent <- popAgentSTM sGrabQueue fn1
                  case mAgent of
                    Nothing    -> retrySTM
                    Just agent -> pure [agent]

          runTask 0 $ runSchedPool pool schedJob
          IOMap.insert fn pool sSchedPoolList
          pure pool
        Just pool -> pure pool

      Pool.spawn pool job

  where fn = getFuncName job


canRun :: MonadIO m => FuncName -> SchedT db m Bool
canRun fn = asks sFuncStatList >>= flip canRun_ fn

canRun_ :: MonadIO m => FuncStatList -> FuncName -> m Bool
canRun_ stList fn = do
  st0 <- IOMap.lookup fn stList
  case st0 of
    Nothing                  -> pure False
    Just FuncStat{sWorker=0} -> pure False
    Just _                   -> pure True


schedJob :: (MonadIO m, Persist db) => Job -> (Nid, Msgid) -> SchedT db m ()
schedJob job (nid, msgid) = do
  assignJob <- asks sAssignJob
  assignJobTime <- asks sAssignJobTime
  persist <- asks sPersist
  liftIO $ P.updateState persist Running fn jn
  t <- liftIO getUnixTime
  IOMap.insert jh t assignJobTime
  r <- liftIO $ assignJob nid msgid job
  if r then pure ()
       else do
         liftIO $ P.updateState persist Pending fn jn
         IOMap.delete jh assignJobTime
         pushChanJob job

  where fn = getFuncName job
        jn = getName job
        jh = getHandle job


adjustFuncStat :: (MonadIO m, Persist db) => FuncName -> SchedT db m ()
adjustFuncStat fn = do
  SchedEnv{..} <- ask
  size <- liftIO $ P.size sPersist Pending fn
  sizePQ <- liftIO $ P.size sPersist Running fn
  sizeL <- liftIO $ P.size sPersist Locked fn
  sc <- liftIO $ P.minSchedAt sPersist fn

  schedAt <- if sc > 0 then pure sc else getEpochTime

  IOMap.alter (update (size + sizePQ + sizeL) sizePQ sizeL schedAt) fn sFuncStatList

  where update :: Int64 -> Int64 -> Int64 -> Int64 -> Maybe FuncStat -> Maybe FuncStat
        update size sizePQ sizeL schedAt st =
          Just ((fromMaybe (funcStat fn) st) { sJob = size
                                             , sRunning = sizePQ
                                             , sLocked = sizeL
                                             , sSchedAt = schedAt
                                             })

removeJob :: (MonadIO m, Persist db) => Job -> SchedT db m ()
removeJob job = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("removeJob: " ++ show (getHandle job))
  t0 <- liftIO getUnixTime
  p <- asks sPersist
  liftIO $ P.delete p fn jn

  asks sSchedPoolList >>= IOMap.lookup fn >>= mapM_ (`Pool.unspawn` job)

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
  pushPollJob

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


failJob :: (MonadUnliftIO m, Persist db) => JobHandle -> SchedT db m ()
failJob jh = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("failJob: " ++ show jh)
  dura <- getJobDuration jh
  runHook eventFailJob jh dura
  releaseLock' jh
  isWaiting <- existsWaitList jh
  if isWaiting then do
    doneJob_ jh "failed"
  else do
    p <- asks sPersist
    mJob <- liftIO $ P.getOne p Running fn jn
    mapM_ (retryLater 1) mJob

  where (fn, jn) = unHandle jh

retryLater :: (MonadIO m, Persist db) => Int64 -> Job -> SchedT db m ()
retryLater later job = do
  nextSchedAt <- (later +) <$> getEpochTime
  p <- asks sPersist
  liftIO $ P.insert p Pending $ setSchedAt nextSchedAt job

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


doneJob_
  :: (MonadIO m, Persist db)
  => JobHandle -> ByteString -> SchedT db m ()
doneJob_ jh w = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("doneJob: " ++ show jh)
  p <- asks sPersist
  getJobDuration jh >>= runHook eventDoneJob jh
  liftIO $ P.delete p fn jn
  pushResult jh w
  where (fn, jn) = unHandle jh


doneJob
  :: (MonadUnliftIO m, Persist db)
  => JobHandle -> ByteString -> SchedT db m ()
doneJob jh w = do
  doneJob_ jh w
  releaseLock' jh

schedLaterJob
  :: (MonadUnliftIO m, Persist db)
  => JobHandle -> Int64 -> Int -> SchedT db m ()
schedLaterJob jh later step = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("schedLaterJob: " ++ show jh)
  releaseLock' jh
  isWaiting <- existsWaitList jh
  if isWaiting then do
    doneJob_ jh "failed"
  else do
    p <- asks sPersist
    mJob <- liftIO $ P.getOne p Running fn jn
    mapM_ (\job -> retryLater later $ setCount (getCount job + step) job) mJob
    getJobDuration jh >>= runHook eventSchedLaterJob jh

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
    j <- liftIO $ P.getOne p Running fn jn
    case j of
      Nothing -> pure True
      Just _ -> do
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

        unless r $ liftIO $ P.updateState p Locked fn jn
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
      j <- liftIO $ P.getOne p Locked fn jn
      case j of
        Nothing  -> releaseLock_ name hh
        Just job -> do
          liftIO $ P.updateState p Pending fn jn
          pushChanJob job

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
  jobs <- liftIO $ filter (check now tout) <$> P.getRunningJob p (now - tout)
  mapM_ (failJob . getHandle) jobs

  where check :: Int64 -> Int64 -> Job -> Bool
        check now t0 job = getSchedAt job + fromIntegral tout < now
          where tout = getJobTimeout (fromIntegral t0) job

getJobTimeout :: Int -> Job -> Int
getJobTimeout t0 job
  | getTimeout job > 0 = getTimeout job
  | otherwise          = t0

revertLockedQueue :: (MonadUnliftIO m, Persist db) => SchedT db m ()
revertLockedQueue = mapM_ checkAndReleaseLock =<< liftIO . P.funcList =<< asks sPersist

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
  pushData <- asks sPushData
  ex <- fmap fromIntegral . readTVarIO =<< asks sExpiration
  waiters <- atomically $ do
    (ks, vs) <- IOMapS.foldrWithKey (foldFunc (check (now - ex))) ([],[]) wl
    mapM_ (`IOMapS.delete` wl) ks
    pure vs

  liftIO $ forM_ waiters $ \(nid, msgid) -> pushData nid msgid ""

  where foldFunc
          :: (WaitItem -> Bool)
          -> JobHandle -> WaitItem
          -> ([JobHandle], [Waiter])
          -> ([JobHandle], [Waiter])
        foldFunc f jh v (acc0, acc1) | f v = (jh : acc0, itemWaiters v ++ acc1)
                                     | otherwise = (acc0, acc1)

        check :: Int64 -> WaitItem -> Bool
        check t0 item = itemTs item < t0

shutdown :: (MonadUnliftIO m) => SchedT db m ()
shutdown = do
  liftIO $ infoM "Periodic.Server.Scheduler" "Scheduler shutdown"
  SchedEnv{..} <- ask
  IOMap.elems sSchedPoolList >>= mapM_ Pool.close
  readTVarIO sTaskList >>= mapM_ cancel
  liftIO sCleanup

prepareWait :: MonadIO m => Job -> Nid -> Msgid -> SchedT db m ()
prepareWait job nid msgid = do
  wl <- asks sWaitList
  now <- getEpochTime
  IOMap.alter (updateWL now) jh wl
  where updateWL :: Int64 -> Maybe WaitItem -> Maybe WaitItem
        updateWL now Nothing     = Just $ WaitItem {itemTs = now, itemWaiters = [waiter]}
        updateWL now (Just item) = Just $ item {itemTs = now, itemWaiters = waiter:itemWaiters item}

        jh = getHandle job
        waiter = (nid, msgid)


pushResult
  :: MonadIO m
  => JobHandle -> ByteString -> SchedT db m ()
pushResult jh w = do
  wl <- asks sWaitList
  pushData <- asks sPushData
  waiters <- atomically $ do
    w0 <- IOMapS.lookup jh wl
    IOMapS.delete jh wl

    case w0 of
      Nothing   -> pure []
      Just item -> pure $ itemWaiters item

  liftIO $ forM_ waiters $ \(nid, msgid) -> pushData nid msgid w

existsWaitList :: MonadIO m => JobHandle -> SchedT db m Bool
existsWaitList jh = do
  wl <- asks sWaitList
  isJust <$> IOMap.lookup jh wl

runHook :: (MonadIO m, GetHookName a) => HookEvent -> a -> Double -> SchedT db m ()
runHook evt n c = do
  hook <- asks sHook
  Hook.runHook hook evt n c
