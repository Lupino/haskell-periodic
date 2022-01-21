{-# LANGUAGE RecordWildCards #-}

module Periodic.Server.SchedPool
  ( SchedPool
  , newSchedPool
  , getHandleList
  , runSchedPool
  , unspawn
  , spawn
  , close
  ) where


import           Control.Monad       (forM_, forever, unless, when)
import           Control.Monad.Cont  (callCC, lift, runContT)
import           Control.Monad.ListM (takeWhileM)
import           Data.IOMap          (IOMap)
import qualified Data.IOMap          as IOMap
import qualified Data.IOMap.STM      as IOMapS
import           Data.Int            (Int64)
import           Data.Map.Strict     (filterWithKey)
import           Metro.Utils         (getEpochTime)
import           Periodic.Types      (Msgid, Nid)
import           Periodic.Types.Job
import           UnliftIO


data SchedJob = SchedJob
  { schedJob   :: Job
  , schedDelay :: TVar Bool
  -- when true job is sched
  }


data SchedState = SchedState
  { stateIsBusy :: Bool
  , stateJob    :: Maybe (TVar SchedJob)
  , stateAlive  :: Bool
  }


type PoolerState = TVar SchedState
type FreeStates = TVar [PoolerState]


data SchedPooler = SchedPooler
  { poolerState :: PoolerState
  , poolerIO    :: Async ()
  }

type LastState = TVar (Maybe PoolerState)

data SchedPool = SchedPool
  { poolerList  :: TVar [SchedPooler]
  , lastState   :: LastState
  , freeStates  :: FreeStates
  , waitingJob  :: TVar [TVar SchedJob]
  , maxWaitSize :: TVar Int
  , waitingLock :: TMVar ()
  , jobList     :: IOMap JobHandle (TVar SchedJob)
  , maxPoolSize :: TVar Int
  , poolSize    :: TVar Int
  , onFree      :: STM ()
  , prepareWork :: FuncName -> STM [(Nid, Msgid)]
  , newState    :: LastState
  }


newSchedPool
  :: MonadIO m
  => TVar Int
  -> TVar Int
  -> STM ()
  -> (FuncName -> STM [(Nid, Msgid)])
  -> m SchedPool
newSchedPool maxPoolSize maxWaitSize onFree prepareWork = do
  poolerList  <- newTVarIO []
  lastState   <- newTVarIO Nothing
  freeStates  <- newTVarIO []
  waitingJob  <- newTVarIO []
  waitingLock <- newTMVarIO ()
  poolSize    <- newTVarIO 0
  jobList     <- IOMap.empty
  newState    <- newTVarIO Nothing
  pure SchedPool {..}


findPoolerState :: MonadIO m => SchedPool -> Job -> m (Maybe PoolerState)
findPoolerState SchedPool {..} job =
  atomically $ readTVar poolerList >>= go
  where go :: [SchedPooler] -> STM (Maybe PoolerState)
        go [] = pure Nothing
        go (x:xs) = do
          state <- readTVar $ poolerState x
          case stateJob state of
            Nothing -> go xs
            Just sjt -> do
              sj <- readTVar sjt
              if getHandle (schedJob sj) == jh then do
                delay <- readTVar $ schedDelay sj
                if delay then pure Nothing
                         else pure $ Just $ poolerState x
              else go xs

        jh = getHandle job


getHandleList :: MonadIO m => SchedPool -> m [JobHandle]
getHandleList SchedPool {..} = atomically $ do
  takeTMVar waitingLock
  handles0 <- readTVar poolerList >>= go
  handles1 <- readTVar waitingJob
  handles <- mapM toHandle $ handles0 ++ handles1
  IOMapS.modifyIOMap (filterWithKey (\k _ -> k `elem` handles)) jobList
  putTMVar waitingLock ()
  pure handles
  where go :: [SchedPooler] -> STM [TVar SchedJob]
        go [] = pure []
        go (x:xs) = do
          state <- readTVar $ poolerState x
          case stateJob state of
            Nothing -> go xs
            Just j -> do
              ys <- go xs
              pure $ j : ys

        toHandle :: TVar SchedJob -> STM JobHandle
        toHandle sJob = getHandle . schedJob <$> readTVar sJob


getFreeState :: FreeStates -> STM (Maybe PoolerState)
getFreeState freeStates = do
  states <- readTVar freeStates
  case states of
    [] -> pure Nothing
    (x:xs) -> do
      writeTVar freeStates xs
      pure $ Just x

getLastPooler :: TVar [SchedPooler] -> STM (Maybe SchedPooler)
getLastPooler pl = readTVar pl >>= go Nothing

  where go :: Maybe SchedPooler -> [SchedPooler] -> STM (Maybe SchedPooler)
        go mPooler [] = pure mPooler
        go mPooler (x:xs) = do
          newMPooler <- comp mPooler x
          go newMPooler xs

        comp :: Maybe SchedPooler -> SchedPooler -> STM (Maybe SchedPooler)
        comp Nothing pooler = do
          mStateJob <- stateJob <$> readTVar (poolerState pooler)
          case mStateJob of
            Nothing -> pure Nothing
            Just sJob -> do
              delay <- schedDelay <$> readTVar sJob
              st <- readTVar delay
              if st then pure Nothing
                    else pure $ Just pooler
        comp (Just pooler0) pooler1 = do
          s0 <- getStateSchedAt $ poolerState pooler0
          s1 <- getStateSchedAt $ poolerState pooler1
          if s0 > s1 then pure $ Just pooler0
                     else pure $ Just pooler1

getStateSchedAt :: PoolerState -> STM Int64
getStateSchedAt state = do
  mJob <- stateJob <$> readTVar state
  case mJob of
    Nothing  -> pure 0
    Just job -> getSchedAt . schedJob <$> readTVar job


getLastStateSchedAt :: LastState -> STM Int64
getLastStateSchedAt ls = do
  mState <- readTVar ls
  case mState of
    Nothing    -> pure 0
    Just state -> getStateSchedAt state


swapLastState :: LastState -> PoolerState -> STM ()
swapLastState ls = writeTVar ls . Just


trySwapLastState :: LastState -> PoolerState -> Int64 -> STM ()
trySwapLastState ls newState schedAt = do
  lastSchedAt <- getLastStateSchedAt ls
  when (lastSchedAt < schedAt) $
    swapLastState ls newState


startPoolerIO
  :: MonadUnliftIO m
  => SchedPool
  -> (Job -> (Nid, Msgid) -> m ())
  -> PoolerState -> m (Async ())
startPoolerIO pool@SchedPool {..} work state =
  async $ (`runContT` pure) $ callCC $ \exit -> forever $ do
    (job, agents) <- atomically $ do
      v <- readTVar state
      case stateJob v of
        Nothing  -> retrySTM
        Just jobT -> do
          job <- readTVar jobT
          canDo <- readTVar $ schedDelay job
          if canDo then do
            agents <- prepareWork $ getFuncName $ schedJob job
            pure (schedJob job, agents)
          else retrySTM

    mapM_ (lift . work job) agents

    finishPoolerState pool state

    s <- readTVarIO state
    unless (stateAlive s) $ exit ()


runSchedPool
  :: MonadUnliftIO m
  => SchedPool
  -> (Job -> (Nid, Msgid) -> m ())
  -> m ()
runSchedPool pool@SchedPool {..} work = do
  state <- atomically $ do
    mState <- readTVar newState
    case mState of
      Nothing    -> retrySTM
      Just state -> do
        writeTVar newState Nothing
        pure state

  io <- startPoolerIO pool work state
  atomically $ modifyTVar' poolerList (++[SchedPooler state io])

finishPoolerState :: MonadIO m => SchedPool -> PoolerState -> m ()
finishPoolerState SchedPool {..} state = atomically $ do
  v <- readTVar state
  case stateJob v of
    Nothing -> pure ()
    Just jobT -> do
      job <- schedJob <$> readTVar jobT
      IOMapS.delete (getHandle job) jobList

  when (stateAlive v) $ do
    takeTMVar waitingLock
    jobs <- readTVar waitingJob
    case jobs of
      [] -> onFree
      (x:xs) -> do
        writeTVar waitingJob xs
        writeTVar state v
          { stateJob = Just x
          }
    putTMVar waitingLock ()


unspawn :: MonadIO m => SchedPool -> Job -> m ()
unspawn pool job = findPoolerState pool job >>= mapM_ (finishPoolerState pool)


insertSchedJob :: SchedPool -> TVar SchedJob -> Int64 -> STM ()
insertSchedJob SchedPool {..} s schedAt = do
  takeTMVar waitingLock
  sJobs <- readTVar waitingJob
  hJobs <- takeWhileM compSchedAt sJobs
  maxWait <- readTVar maxWaitSize
  writeTVar waitingJob $! take maxWait $ hJobs ++ [s] ++ drop (length hJobs) sJobs
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
          }
        IOMapS.insert jh sJob jobList
        mFreeState <- getFreeState freeStates
        case mFreeState of
          Just state -> do
            modifyTVar' state $ genPoolerState sJob
            trySwapLastState lastState state schedAt
          Nothing -> do
            size <- readTVar poolSize
            maxSize <- readTVar maxPoolSize
            when (maxSize < size) $ do
              writeTVar poolSize maxSize
              poolers <- readTVar poolerList
              writeTVar poolerList $! drop (size - maxSize) poolers
              forM_ (take (size - maxSize) poolers) $ \p ->
                modifyTVar' (poolerState p) $ \st -> st {stateAlive = False}

            if size < maxSize then do
              state <- newTVar SchedState
                { stateIsBusy = True
                , stateJob    = Just sJob
                , stateAlive  = True
                }

              writeTVar newState $ Just state

              modifyTVar' poolSize (+1)
              trySwapLastState lastState state schedAt
            else do
              lastSchedAt <- getLastStateSchedAt lastState
              if lastSchedAt < 1000 || (lastSchedAt > now + 1 && lastSchedAt > schedAt) then do
                mLastState <- readTVar lastState
                case mLastState of
                  Nothing -> pure ()
                  Just state -> do
                    mJob <- stateJob <$> readTVar state
                    case mJob of
                      Nothing   -> pure ()
                      Just oJob -> insertSchedJob pool oJob schedAt

                    modifyTVar' state $ genPoolerState sJob

                    mLastPooler <- getLastPooler poolerList
                    case mLastPooler of
                      Nothing -> pure ()
                      Just pooler -> swapLastState lastState (poolerState pooler)

              else insertSchedJob pool sJob schedAt


  where schedAt = getSchedAt job
        jh = getHandle job
        genPoolerState sJob state = state
          { stateIsBusy = True
          , stateJob = Just sJob
          }

        getDelay :: MonadIO m => Int64 -> m (TVar Bool)
        getDelay now
          | schedAt > now + 1 = registerDelay delayUS
          | otherwise         = newTVarIO True
          where delayUS = fromIntegral $ (schedAt - now) * 1000000


close :: MonadIO m => SchedPool -> m ()
close SchedPool {..} = readTVarIO poolerList >>= mapM_ (cancel . poolerIO)
