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
import           Data.List                  (partition, sortOn)
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
import           System.Log.Logger          (debugM, infoM)
import           UnliftIO
import           UnliftIO.Concurrent        (threadDelay)

data Action = Add Job
    | Remove Job
    | PollJob
    | Poll1

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

data PoolState = PoolState
  { poolState :: Bool
  , poolDelay :: TVar Bool  -- when delay true job is sched
  , poolJob   :: Maybe Job
  }

data Pooler = Pooler
  { poolerState  :: TVar PoolState
  , poolerWorker :: Async ()
  }

data SchedEnv db = SchedEnv
    -- main poll loop every time interval
    { sPollInterval   :: TVar Int
    -- revert process queue loop every time interval
    , sRevertInterval :: TVar Int
    -- the task do timeout
    , sTaskTimeout    :: TVar Int
    -- lock timeout
    , sLockTimeout    :: TVar Int
    -- max poll batch size
    , sMaxBatchSize   :: TVar Int
    -- client or worker keepalive
    , sKeepalive      :: TVar Int
    -- run job cache expiration
    , sExpiration     :: TVar Int
    , sCleanup        :: IO ()
    , sFuncStatList   :: FuncStatList
    , sLocker         :: L.Lock
    , sGrabQueue      :: GrabQueue
    -- sched state, when false sched is exited.
    , sAlive          :: TVar Bool
    , sChanList       :: TVar [Action]
    , sWaitList       :: WaitList
    , sLockList       :: LockList
    , sPersist        :: db
    , sAssignJob      :: Nid -> Msgid -> Job -> IO Bool
    , sPushData       :: Nid -> Msgid -> ByteString -> IO ()
    , sHook           :: Hook
    , sAssignJobTime  :: IOMap JobHandle UnixTime
    , sPoolerList     :: TVar [Pooler]
    , sLastPoolState  :: TVar (Maybe (TVar PoolState))
    , sFreePoolStates :: TVar [TVar PoolState]
    , sQuickRunJob    :: TVar [Job]
    , sWaitingJob     :: TVar [Job]
    , sMaxPoolSize    :: TVar Int
    , sPoolSize       :: TVar Int
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
  -> m (SchedEnv db)
initSchedEnv config sGrabQueue sC sAssignJob sPushData sHook = do
  sFuncStatList   <- IOMap.empty
  sWaitList       <- IOMap.empty
  sLockList       <- IOMap.empty
  sLocker         <- L.new
  sAlive          <- newTVarIO True
  sChanList       <- newTVarIO []
  sPollInterval   <- newTVarIO 300
  sRevertInterval <- newTVarIO 300
  sTaskTimeout    <- newTVarIO 600
  sLockTimeout    <- newTVarIO 300
  sMaxBatchSize   <- newTVarIO 10
  sKeepalive      <- newTVarIO 300
  sExpiration     <- newTVarIO 300
  sCleanup        <- toIO sC
  sPersist        <- liftIO $ P.newPersist config
  sAssignJobTime  <- IOMap.empty
  sPoolerList     <- newTVarIO []
  sLastPoolState  <- newTVarIO Nothing
  sFreePoolStates <- newTVarIO []
  sQuickRunJob    <- newTVarIO []
  sWaitingJob     <- newTVarIO []
  sMaxPoolSize    <- newTVarIO 10
  sPoolSize       <- newTVarIO 0
  pure SchedEnv{..}

startSchedT :: (MonadUnliftIO m, Persist db) => SchedT db m ()
startSchedT = do
  liftIO $ infoM "Periodic.Server.Scheduler" "Scheduler started"
  SchedEnv{..} <- ask
  runTask_ sRevertInterval revertRunningQueue
  runTask_ sPollInterval $ pushChanList Poll1
  runTask 0 runChanJob
  runTask 100 purgeExpired
  runTask 60 revertLockedQueue
  runTask 100 purgeEmptyLock

  loadInt "poll-interval" sPollInterval
  loadInt "revert-interval" sRevertInterval
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
    "poll-interval"   -> saveInt "poll-interval" val sPollInterval
    "revert-interval" -> saveInt "revert-interval" val sRevertInterval
    "timeout"         -> saveInt "timeout" val sTaskTimeout
    "lock-timeout"    -> saveInt "lock-timeout" val sLockTimeout
    "keepalive"       -> saveInt "keepalive" val sKeepalive
    "max-batch-size"  -> saveInt "max-batch-size" val sMaxBatchSize
    "max-pool-size"   -> saveInt "max-pool-size" val sMaxPoolSize
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
    "max-pool-size"   -> readTVarIO sMaxPoolSize
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

runChanJob :: (MonadUnliftIO m, Persist db) => SchedT db m ()
runChanJob = do
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
        doChanJob (Add job)    = reSchedJob job
        doChanJob (Remove job) = findPooler job >>= mapM_ removePoolerJob
        doChanJob PollJob      = pollJob0
        doChanJob Poll1        = pollJob1


pollJob0 :: (MonadUnliftIO m, Persist db) => SchedT db m ()
pollJob0 = do
  funcList <- getAvaliableFuncList
  next <- getNextPoll
  pollJob_ funcList next


pollJob1 :: (MonadUnliftIO m, Persist db) => SchedT db m ()
pollJob1 = do
  now <- getEpochTime
  waitingJob <- asks sWaitingJob
  (jobs, wJobs) <- atomically $ do
    (jobs, wJobs) <- partition (\x -> getSchedAt x <= now ) <$> readTVar waitingJob

    if length jobs > 0 then do
      writeTVar waitingJob wJobs
      pure (jobs, [])
    else do
      let sorted = sortOn getSchedAt wJobs
      writeTVar waitingJob $! drop 1 sorted
      pure ([], take 1 sorted)

  case (jobs, wJobs) of
    ([], xs) -> mapM_ reSchedJob xs
    (xs, []) -> mapM_ reSchedJob xs
    _        -> do
      size <- readTVarIO =<< asks sPoolSize
      funcList <- getAvaliableFuncList
      next <- getNextPoll
      p <- asks sPersist
      count <- liftIO $ P.countPending p funcList next
      when (count > size) $ pollJob_ funcList next


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
  => [FuncName] -> Int64 -> SchedT db m ()
pollJob_ [] _ = pure ()
pollJob_ funcList next = do
  handles <- getHandleList

  maxBatchSize <- readTVarIO =<< asks sMaxBatchSize
  p <- asks sPersist
  jobs <- liftIO $ P.getPendingJob p funcList next
            (maxBatchSize + length handles)

  mapM_ reSchedJob $ filter (flip notElem handles . getHandle) jobs


pushChanList :: MonadIO m => Action -> SchedT db m ()
pushChanList act = do
  cl <- asks sChanList
  atomically $ modifyTVar' cl (act:)


runJob :: (MonadIO m, Persist db) => Job -> SchedT db m ()
runJob job = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("runJob: " ++ show (getHandle job))
  SchedEnv{..} <- ask
  mAgent <- atomically $ popAgentSTM sGrabQueue fn
  case mAgent of
    Nothing -> pushJob job
    Just (nid, msgid) -> do
      liftIO $ P.insert sPersist Running fn jn job
      t <- liftIO getUnixTime
      IOMap.insert (getHandle job) t sAssignJobTime
      r <- liftIO $ sAssignJob nid msgid job
      if r then pure ()
           else pushJob job

  where fn = getFuncName job
        jn = getName job


pushJob :: (MonadIO m, Persist db) => Job -> SchedT db m ()
pushJob job = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("pushJob: " ++ show (getHandle job))
  t0 <- liftIO getUnixTime
  p <- asks sPersist
  isRunning <- liftIO $ isJust <$> P.getOne p Running fn jn
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

reSchedJob :: (MonadUnliftIO m, Persist db) => Job -> SchedT db m ()
reSchedJob job = do
  next <- getNextPoll
  when (getSchedAt job < next) $ do
    r <- canRun $ getFuncName job
    when r $ schedJob job

findPooler :: MonadIO m => Job -> SchedT db m (Maybe Pooler)
findPooler job = do
  poolerList <- asks sPoolerList
  atomically $ readTVar poolerList >>= go
  where go :: [Pooler] -> STM (Maybe Pooler)
        go [] = pure Nothing
        go (x:xs) = do
          state <- readTVar $ poolerState x
          case poolJob state of
            Nothing -> go xs
            Just j ->
              if getHandle j == jh then do
                delay <- readTVar $ poolDelay state
                if delay then pure Nothing
                         else pure $ Just x
              else go xs

        jh = getHandle job


getHandleList :: MonadIO m => SchedT db m [JobHandle]
getHandleList = do
  poolerList  <- asks sPoolerList
  quickRunJob <- asks sQuickRunJob
  waitingJob  <- asks sWaitingJob
  atomically $ do
    handles0 <- readTVar poolerList >>= go
    handles1 <- map getHandle <$> readTVar quickRunJob
    handles2 <- map getHandle <$> readTVar waitingJob
    pure $ handles0 ++ handles1 ++ handles2
  where go :: [Pooler] -> STM [JobHandle]
        go [] = pure []
        go (x:xs) = do
          state <- readTVar $ poolerState x
          case poolJob state of
            Nothing -> go xs
            Just j -> do
              delay <- readTVar $ poolDelay state
              if delay then go xs
                       else do
                         handles <- go xs
                         pure $ getHandle j : handles


removePoolerJob :: MonadIO m => Pooler -> SchedT db m ()
removePoolerJob pooler = do
  poolSize <- asks sPoolSize
  stateList <- asks sFreePoolStates
  atomically $ do
    modifyTVar' (poolerState pooler) $ \st -> st
      { poolJob = Nothing
      , poolState = False
      }
    modifyTVar' poolSize (\v -> v - 1)
    modifyTVar' stateList (poolerState pooler:)


canRun :: MonadIO m => FuncName -> SchedT db m Bool
canRun fn = asks sFuncStatList >>= flip canRun_ fn

canRun_ :: MonadIO m => FuncStatList -> FuncName -> m Bool
canRun_ stList fn = do
  st0 <- IOMap.lookup fn stList
  case st0 of
    Nothing                  -> pure False
    Just FuncStat{sWorker=0} -> pure False
    Just _                   -> pure True


getOnePoolState :: MonadIO m => TVar [TVar PoolState] -> m (Maybe (TVar PoolState))
getOnePoolState poolStateList = atomically $ do
  states <- readTVar poolStateList
  case states of
    [] -> pure Nothing
    (x:xs) -> do
      writeTVar poolStateList $! xs
      pure $ Just x


getPooler :: MonadIO m => TVar [Pooler] -> m (Maybe Pooler, Maybe Pooler)
getPooler poolerList = do
  atomically $ readTVar poolerList >>= go [] Nothing

  where go :: [Pooler] -> Maybe Pooler -> [Pooler] -> STM (Maybe Pooler, Maybe Pooler)
        go _ mPooler [] = pure (Nothing, mPooler)
        go ys mPooler (x:xs) = do
          state <- readTVar $ poolerState x
          if poolState state then do
                               newMPooler <- comp mPooler x
                               go (ys ++ [x]) newMPooler xs
                             else do
                               writeTVar (poolerState x) $ state {poolState=True}
                               writeTVar poolerList $! (xs ++ ys ++ [x])
                               pure (Just x, Nothing)

        comp :: Maybe Pooler -> Pooler -> STM (Maybe Pooler)
        comp Nothing pooler = do
          delay <- poolDelay <$> readTVar (poolerState pooler)
          st <- readTVar delay
          if st then pure Nothing
                else pure $ Just pooler
        comp (Just pooler0) pooler1 = do
          s0 <- getPoolerSchedAt pooler0
          s1 <- getPoolerSchedAt pooler1
          if s0 > s1 then pure $ Just pooler0
                     else pure $ Just pooler1

getLastPooler :: TVar [Pooler] -> STM (Maybe Pooler)
getLastPooler poolerList = readTVar poolerList >>= go Nothing

  where go :: Maybe Pooler -> [Pooler] -> STM (Maybe Pooler)
        go mPooler [] = pure mPooler
        go mPooler (x:xs) = do
          newMPooler <- comp mPooler x
          go newMPooler xs

        comp :: Maybe Pooler -> Pooler -> STM (Maybe Pooler)
        comp Nothing pooler = do
          delay <- poolDelay <$> readTVar (poolerState pooler)
          st <- readTVar delay
          if st then pure Nothing
                else pure $ Just pooler
        comp (Just pooler0) pooler1 = do
          s0 <- getPoolerSchedAt pooler0
          s1 <- getPoolerSchedAt pooler1
          if s0 > s1 then pure $ Just pooler0
                     else pure $ Just pooler1

getPoolerSchedAt :: Pooler -> STM Int64
getPoolerSchedAt pooler =
  maybe 0 getSchedAt . poolJob <$> readTVar (poolerState pooler)


getLastPoolStateSchedAt :: TVar (Maybe (TVar PoolState)) -> STM Int64
getLastPoolStateSchedAt lastPoolState = do
  mPoolState <- readTVar lastPoolState
  case mPoolState of
    Nothing -> pure 0
    Just ps -> maybe 0 getSchedAt . poolJob <$> readTVar ps


swapLastPoolState :: TVar (Maybe (TVar PoolState)) -> TVar PoolState -> STM ()
swapLastPoolState lastPoolState = writeTVar lastPoolState . Just


trySwapLastPoolState :: TVar (Maybe (TVar PoolState)) -> TVar PoolState -> Int64 -> STM ()
trySwapLastPoolState lastPoolState newPoolState schedAt = do
  lastSchedAt <- getLastPoolStateSchedAt lastPoolState
  when (lastSchedAt < schedAt) $
    swapLastPoolState lastPoolState newPoolState


startPoolWorker
  :: (MonadUnliftIO m, Persist db)
  => TVar PoolState -> SchedT db m (Async ())
startPoolWorker h = do
  funcStatList <- asks sFuncStatList
  grabQueue <- asks sGrabQueue
  poolSize <- asks sPoolSize
  stateList <- asks sFreePoolStates
  quickRunJob <- asks sQuickRunJob
  chanList <- asks sChanList
  async $ forever $ do
    mTask <- atomically $ do
      v <- readTVar h
      case poolJob v of
        Nothing  -> retrySTM
        Just job -> do
          canDo <- readTVar $ poolDelay v
          if canDo then do
            mFuncStat <- IOMapS.lookup (getFuncName job) funcStatList
            case mFuncStat of
              Nothing                  -> pure Nothing
              Just FuncStat{sWorker=0} -> pure Nothing
              Just st -> do
                if sBroadcast st then do
                  agents <- popAgentListSTM grabQueue (getFuncName job)
                  pure $ Just (job, agents, True)
                else do
                  mAgent <- popAgentSTM grabQueue (getFuncName job)
                  case mAgent of
                    Nothing    -> retrySTM
                    Just agent -> pure $ Just (job, [agent], False)

          else retrySTM

    case mTask of
      Nothing                -> pure ()
      Just (job, agents, bc) -> mapM_ (schedJob_ job bc) agents

    atomically $ do
      jobs <- readTVar quickRunJob
      case jobs of
        [] -> do
          modifyTVar' h $ \v -> v
            { poolJob   = Nothing
            , poolState = False
            }
          modifyTVar' poolSize (\v -> v - 1)
          modifyTVar' stateList (h:)
          modifyTVar' chanList (Poll1:)
        (x:xs) -> do
          writeTVar quickRunJob xs
          modifyTVar' h $ \v -> v
            { poolJob = Just x
            }


schedJob
  :: (MonadUnliftIO m, Persist db)
  => Job -> SchedT db m ()
schedJob job = do
  poolerList <- asks sPoolerList
  poolSize <- asks sPoolSize
  mPoolState <- getOnePoolState =<< asks sFreePoolStates
  lastPoolState <- asks sLastPoolState
  quickRunJob <- asks sQuickRunJob
  waitingJob <- asks sWaitingJob
  now <- getEpochTime
  case mPoolState of
    Just ps -> do
      delay <- getDelay now
      atomically $ do
        writeTVar ps $ genPoolState delay
        modifyTVar' poolSize (+1)
        trySwapLastPoolState lastPoolState ps schedAt
    Nothing -> do
      size <- length <$> readTVarIO poolerList
      maxSise <- readTVarIO =<< asks sMaxPoolSize
      if size < maxSise then do
        delay <- getDelay now
        state <- newTVarIO $ genPoolState delay
        io <- startPoolWorker state
        atomically $ do
          modifyTVar' poolerList (++[Pooler state io])
          modifyTVar' poolSize (+1)
          trySwapLastPoolState lastPoolState state schedAt
      else do
        mState <- atomically $ do
          lastSchedAt <- getLastPoolStateSchedAt lastPoolState
          if lastSchedAt > schedAt then do
            delay <- newTVar False
            mLastPoolState <- readTVar lastPoolState
            case mLastPoolState of
              Nothing -> pure Nothing
              Just state -> do
                mJob <- poolJob <$> readTVar state
                case mJob of
                  Nothing   -> pure ()
                  Just oJob -> modifyTVar' waitingJob (oJob:)

                writeTVar state $ genPoolState delay
                mLastPooler <- getLastPooler poolerList
                case mLastPooler of
                  Nothing -> pure $ Just state
                  Just pooler -> do
                    swapLastPoolState lastPoolState (poolerState pooler)
                    pure $ Just state
          else do
            if schedAt <= now + 1 then modifyTVar' quickRunJob (job:)
                                  else modifyTVar' waitingJob (job:)

            pure Nothing
        case mState of
          Nothing -> pure ()
          Just state -> do
            delay <- getDelay now
            atomically $ modifyTVar' state $ \v -> v {poolDelay = delay}

  where schedAt = getSchedAt job
        genPoolState delay = PoolState
          { poolState = True
          , poolJob = Just job
          , poolDelay = delay
          }
        getDelay :: MonadIO m => Int64 -> m (TVar Bool)
        getDelay now
          | schedAt > now + 1 = registerDelay delayUS
          | otherwise         = newTVarIO True
          where delayUS = fromIntegral $ (schedAt - now) * 1000000

schedJob_ :: (MonadIO m, Persist db) => Job -> Bool -> (Nid, Msgid) -> SchedT db m ()
schedJob_ job True (nid, msgid) = do
  assignJob <- asks sAssignJob
  liftIO . void $ assignJob nid msgid job
schedJob_ job False (nid, msgid) = do
  assignJob <- asks sAssignJob
  assignJobTime <- asks sAssignJobTime
  persist <- asks sPersist
  liftIO $ P.insert persist Running fn jn job
  t <- liftIO getUnixTime
  IOMap.insert (getHandle job) t assignJobTime
  r <- liftIO $ assignJob nid msgid job
  if r then pure ()
       else do
         liftIO $ P.insert persist Pending fn jn job
         pushChanList (Add job)

  where fn = getFuncName job
        jn = getName job


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


failJob :: (MonadUnliftIO m, Persist db) => JobHandle -> SchedT db m ()
failJob jh = do
  liftIO $ debugM "Periodic.Server.Scheduler" ("failJob: " ++ show jh)
  dura <- getJobDuration jh
  runHook eventFailJob jh dura
  releaseLock' jh
  isWaiting <- existsWaitList jh
  if isWaiting then do
    doneJob jh ""
  else do
    p <- asks sPersist
    job <- liftIO $ P.getOne p Running fn jn
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
    doneJob jh ""
  else do
    p <- asks sPersist
    job <- liftIO $ P.getOne p Running fn jn
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
    j <- liftIO $ P.getOne p Running fn jn
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

        unless r $ liftIO $ P.insert p Locked fn jn job
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
  readTVarIO sPoolerList >>= mapM_ (cancel . poolerWorker)
  alive <- atomically $ do
    t <- readTVar sAlive
    writeTVar sAlive False
    return t
  when alive . void . async $ liftIO sCleanup

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
