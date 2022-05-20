{-# LANGUAGE RecordWildCards #-}

module Periodic.Server.SchedPool
  ( SchedPool
  , newSchedPool
  , getHandleList
  , runSchedPool
  , getLastSchedAt
  , unspawn
  , spawn
  ) where


import           Control.Monad       (unless, when, (>=>))
import           Control.Monad.ListM (takeWhileM)
import           Data.Int            (Int64)
import           Data.IOMap          (IOMap)
import qualified Data.IOMap          as IOMap
import qualified Data.IOMap.STM      as IOMapS
import           Data.Map.Strict     (filterWithKey)
import           Metro.Utils         (getEpochTime)
import           Periodic.Types      (Msgid, Nid)
import           Periodic.Types.Job
import           UnliftIO


data SchedJob = SchedJob
  { schedJob   :: Job
  , schedDelay :: TVar Bool
  -- when true job is sched
  , schedAlive :: Bool
  }


type PoolerState = TMVar (TVar SchedJob)

data SchedPool = SchedPool
  { waitingJob  :: TVar [TVar SchedJob]
  , maxWaitSize :: TVar Int
  , waitingLock :: TMVar ()
  , jobList     :: IOMap JobHandle (TVar SchedJob)
  , onFree      :: STM ()
  , poolerState :: PoolerState
  }


newSchedPool
  :: MonadIO m
  => TVar Int
  -> STM ()
  -> m SchedPool
newSchedPool maxWaitSize onFree = do
  waitingJob  <- newTVarIO []
  waitingLock <- newTMVarIO ()
  jobList     <- IOMap.empty
  poolerState <- newEmptyTMVarIO
  pure SchedPool {..}


getHandleList :: MonadIO m => SchedPool -> m [JobHandle]
getHandleList SchedPool {..} = atomically $ do
  takeTMVar waitingLock
  handles0 <- go
  handles1 <- readTVar waitingJob
  handles <- mapM toHandle $ handles0 ++ handles1
  IOMapS.modifyIOMap (filterWithKey (\k _ -> k `elem` handles)) jobList
  putTMVar waitingLock ()
  pure handles
  where go :: STM [TVar SchedJob]
        go = do
          mJob <- tryReadTMVar poolerState
          case mJob of
            Nothing -> pure []
            Just j  -> pure [j]

toHandle :: TVar SchedJob -> STM JobHandle
toHandle sJob = getHandle . schedJob <$> readTVar sJob


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
  jobs <- readTVar waitingJob
  case jobs of
    [] -> onFree
    (x:xs) -> do
      r <- tryPutTMVar poolerState x
      when r $ writeTVar waitingJob xs
  putTMVar waitingLock ()


unspawn :: MonadIO m => SchedPool -> Job -> m ()
unspawn SchedPool {..} job = atomically $ do
  mSchedJob <- IOMapS.lookup jh jobList
  case mSchedJob of
    Nothing -> pure ()
    Just sJob -> modifyTVar' sJob $ \v -> v
      { schedAlive = False
      }
  where jh = getHandle job


insertSchedJob :: SchedPool -> TVar SchedJob -> Int64 -> STM ()
insertSchedJob SchedPool {..} s schedAt = do
  takeTMVar waitingLock
  sJobs <- readTVar waitingJob
  hJobs <- takeWhileM compSchedAt sJobs
  maxWait <- readTVar maxWaitSize
  let jobs = hJobs ++ [s] ++ drop (length hJobs) sJobs
      waitJobs = take maxWait jobs
      dropJobs  = drop maxWait jobs
  writeTVar waitingJob $! waitJobs
  mapM_ (toHandle >=> flip IOMapS.delete jobList) dropJobs
  putTMVar waitingLock ()

  where compSchedAt :: TVar SchedJob -> STM Bool
        compSchedAt t = do
          newSchedAt <- getSchedAt . schedJob <$> readTVar t
          pure $ newSchedAt <= schedAt

spawn
  :: MonadIO m
  => SchedPool
  -> Job -> m ()
spawn pool@SchedPool {..} job = do
  now <- getEpochTime
  delay <- getDelay now
  atomically $ do
    mSchedJob <- IOMapS.lookup jh jobList
    case mSchedJob of
      Just sJobT -> do
        sJob <- readTVar sJobT
        oDelay <- readTVar $ schedDelay sJob
        unless oDelay $
          writeTVar sJobT sJob
            { schedDelay = delay
            , schedJob   = job
            }
      Nothing   -> do
        sJob <- newTVar SchedJob
          { schedJob   = job
          , schedDelay = delay
          , schedAlive = True
          }
        IOMapS.insert jh sJob jobList
        lastSchedAt <- getStateSchedAt poolerState
        if lastSchedAt < 1000 || (lastSchedAt > now + 1 && lastSchedAt > schedAt) then do
          mJob <- tryTakeTMVar poolerState
          putTMVar poolerState sJob
          case mJob of
            Nothing   -> pure ()
            Just oJob -> insertSchedJob pool oJob schedAt

        else insertSchedJob pool sJob schedAt


  where schedAt = getSchedAt job
        jh = getHandle job
        -- genPoolerState sJob state = state
        --   { stateJob = Just sJob
        --   }

        getDelay :: MonadIO m => Int64 -> m (TVar Bool)
        getDelay now
          | schedAt > now + 1 = registerDelay delayUS
          | otherwise         = newTVarIO True
          where delayUS = fromIntegral $ (schedAt - now) * 1000000
