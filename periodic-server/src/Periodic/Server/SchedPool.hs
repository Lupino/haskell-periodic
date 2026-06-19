{-# LANGUAGE RecordWildCards #-}

module Periodic.Server.SchedPool
  ( SchedPool
  , newSchedPool
  , getHandleList
  , runSchedPool
  , getLastSchedAt
  , unspawn
  , spawn

  , setPoolerIO
  , cancelSchedPool
  ) where


import           Control.Monad     (unless, when)
import           Data.Int          (Int64)
import           Data.Map.Strict   (Map)
import qualified Data.Map.Strict   as Map
import           Metro.Utils       (getEpochTime)
import           Periodic.Types    (Msgid, Nid)
import           Periodic.Types.Job
import           UnliftIO


data SchedJob = SchedJob
  { schedJob   :: Job
  , schedDelay :: TVar Bool
  -- when true job is sched
  , schedAlive :: Bool
  }


type PoolerState = TMVar (TVar SchedJob)

type WaitingKey = (Int64, JobHandle)

data WaitingState = WaitingState
  { waitingJobs    :: Map WaitingKey (TVar SchedJob)
  , waitingHandles :: Map JobHandle WaitingKey
  }

data SchedPool = SchedPool
  { waitingJob  :: TVar WaitingState
  , maxWaitSize :: TVar Int
  , waitingLock :: TMVar ()
  , onFree      :: STM ()
  , poolerState :: PoolerState
  , poolerIO    :: TMVar (Async ())
  }


newSchedPool
  :: MonadIO m
  => TVar Int
  -> STM ()
  -> m SchedPool
newSchedPool maxWaitSize onFree = do
  waitingJob  <- newTVarIO emptyWaitingState
  waitingLock <- newTMVarIO ()
  poolerState <- newEmptyTMVarIO
  poolerIO <- newEmptyTMVarIO
  pure SchedPool {..}

emptyWaitingState :: WaitingState
emptyWaitingState = WaitingState Map.empty Map.empty


setPoolerIO :: MonadIO m => SchedPool -> Async () -> m ()
setPoolerIO SchedPool {..} = atomically . writeTMVar poolerIO

cancelSchedPool :: MonadUnliftIO m => SchedPool -> m ()
cancelSchedPool SchedPool {..} = mapM_ cancel =<< atomically (tryReadTMVar poolerIO)


getWaitingJob :: SchedPool -> STM [TVar SchedJob]
getWaitingJob SchedPool {..} = do
  handles0 <- go
  handles1 <- Map.elems . waitingJobs <$> readTVar waitingJob
  pure $ handles0 ++ handles1
  where go :: STM [TVar SchedJob]
        go = do
          mJob <- tryReadTMVar poolerState
          case mJob of
            Nothing -> pure []
            Just j  -> pure [j]


getHandleList :: MonadIO m => SchedPool -> m [JobHandle]
getHandleList pool@SchedPool {..} = atomically $ do
  takeTMVar waitingLock
  handles <- mapM toHandle =<< getWaitingJob pool
  putTMVar waitingLock ()
  pure handles


toHandle :: TVar SchedJob -> STM JobHandle
toHandle sJob = getHandle . schedJob <$> readTVar sJob


data SchedJobLocation = ActiveJob (TVar SchedJob) | WaitingJob WaitingKey (TVar SchedJob)

findJob :: SchedPool -> JobHandle -> STM (Maybe SchedJobLocation)
findJob SchedPool {..} jh = do
  mActive <- tryReadTMVar poolerState
  case mActive of
    Just active -> do
      activeHandle <- toHandle active
      if activeHandle == jh then pure $ Just $ ActiveJob active
                            else findWaiting
    Nothing -> findWaiting
  where
    findWaiting = do
      WaitingState {..} <- readTVar waitingJob
      case Map.lookup jh waitingHandles of
        Nothing  -> pure Nothing
        Just key -> pure $ WaitingJob key <$> Map.lookup key waitingJobs


getStateSchedAt :: PoolerState -> STM Int64
getStateSchedAt state = do
  mJob <- tryReadTMVar state
  case mJob of
    Nothing  -> pure 0
    Just job -> getSchedAt . schedJob <$> readTVar job


getLastSchedAt :: MonadIO m => SchedPool -> m Int64
getLastSchedAt SchedPool{..} = atomically $ getStateSchedAt poolerState


runSchedPool
  :: MonadIO m
  => SchedPool
  -> (Job -> (Nid, Msgid) -> m ())
  -> STM [(Nid, Msgid)]
  -> m ()
runSchedPool pool@SchedPool {..} work prepareWork = do
  (job, agents) <- atomically $ do
    jobT <- takeTMVar poolerState
    job <- readTVar jobT
    if schedAlive job then do
      canDo <- readTVar $ schedDelay job
      if canDo then do
        agents <- prepareWork
        pure (schedJob job, agents)
      else retrySTM
    else pure (schedJob job, [])

  mapM_ (work job) agents

  finishPoolerState pool


finishPoolerState :: MonadIO m => SchedPool -> m ()
finishPoolerState SchedPool {..} = atomically $ do
  takeTMVar waitingLock
  waitingState@WaitingState {..} <- readTVar waitingJob
  case Map.minViewWithKey waitingJobs of
    Nothing -> onFree
    Just ((key, x), jobs) -> do
      r <- tryPutTMVar poolerState x
      when r $ writeTVar waitingJob waitingState
        { waitingJobs = jobs
        , waitingHandles = Map.delete (snd key) waitingHandles
        }
      s <- (`div` 4) <$> readTVar maxWaitSize
      when (Map.size jobs < s) onFree
  putTMVar waitingLock ()


unspawn :: MonadIO m => SchedPool -> Job -> m ()
unspawn pool job = atomically $ do
  mSchedJob <- findJob pool jh
  case mSchedJob of
    Nothing -> pure ()
    Just (ActiveJob sJob) -> markDead sJob
    Just (WaitingJob key sJob) -> do
      markDead sJob
      removeWaitingJob pool key jh
  where jh = getHandle job
        markDead sJob = modifyTVar' sJob $ \v -> v { schedAlive = False }


insertSchedJob :: SchedPool -> TVar SchedJob -> STM ()
insertSchedJob SchedPool {..} s = do
  schedJob0 <- readTVar s
  let job = schedJob schedJob0
      jh = getHandle job
      key = (getSchedAt job, jh)
  waitingState@WaitingState {..} <- readTVar waitingJob
  let jobs0 = case Map.lookup jh waitingHandles of
        Nothing     -> waitingJobs
        Just oldKey -> Map.delete oldKey waitingJobs
      jobs1 = Map.insert key s jobs0
      handles1 = Map.insert jh key waitingHandles
  maxWait <- readTVar maxWaitSize
  let (jobs2, handles2) = trimWaiting maxWait jobs1 handles1
  writeTVar waitingJob $! waitingState
    { waitingJobs = jobs2
    , waitingHandles = handles2
    }

trimWaiting :: Int -> Map WaitingKey (TVar SchedJob) -> Map JobHandle WaitingKey -> (Map WaitingKey (TVar SchedJob), Map JobHandle WaitingKey)
trimWaiting maxWait jobs handles
  | Map.size jobs <= maxWait = (jobs, handles)
  | otherwise =
      let ((key, _), jobs') = Map.deleteFindMax jobs
      in trimWaiting maxWait jobs' (Map.delete (snd key) handles)

removeWaitingJob :: SchedPool -> WaitingKey -> JobHandle -> STM ()
removeWaitingJob SchedPool {..} key jh =
  modifyTVar' waitingJob $ \waitingState@WaitingState {..} -> waitingState
    { waitingJobs = Map.delete key waitingJobs
    , waitingHandles = Map.delete jh waitingHandles
    }

spawn
  :: MonadIO m
  => SchedPool
  -> Job -> m ()
spawn pool@SchedPool {..} job = do
  now <- getEpochTime
  delay <- getDelay now
  atomically $ do
    mSchedJob <- findJob pool jh
    case mSchedJob of
      Just loc -> do
        let sJobT = case loc of
              ActiveJob t    -> t
              WaitingJob _ t -> t
        sJob <- readTVar sJobT
        oDelay <- readTVar $ schedDelay sJob
        unless oDelay $ do
          writeTVar sJobT sJob
            { schedDelay = delay
            , schedJob   = job
            }
          case loc of
            ActiveJob _        -> pure ()
            WaitingJob key _ -> do
              removeWaitingJob pool key jh
              takeTMVar waitingLock
              insertSchedJob pool sJobT
              putTMVar waitingLock ()
      Nothing   -> do
        sJob <- newTVar SchedJob
          { schedJob   = job
          , schedDelay = delay
          , schedAlive = True
          }
        lastSchedAt <- getStateSchedAt poolerState
        if lastSchedAt < 1000 || (lastSchedAt > now + 1 && lastSchedAt > schedAt) then do
          mJob <- tryTakeTMVar poolerState
          putTMVar poolerState sJob
          case mJob of
            Nothing   -> pure ()
            Just oJob -> do
              takeTMVar waitingLock
              insertSchedJob pool oJob
              putTMVar waitingLock ()

        else do
          takeTMVar waitingLock
          insertSchedJob pool sJob
          putTMVar waitingLock ()


  where schedAt = getSchedAt job
        jh = getHandle job

        getDelay :: MonadIO m => Int64 -> m (TVar Bool)
        getDelay now
          | schedAt > now + 1 = registerDelay delayUS
          | otherwise         = newTVarIO True
          where delayUS = fromIntegral $ (schedAt - now) * 1000000
