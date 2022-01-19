{-# LANGUAGE RecordWildCards #-}

module Periodic.Server.SchedPool
  ( SchedPool
  , newSchedPool
  , getHandleList
  , unspawn
  , spawn
  , poll
  , close
  ) where


import           Control.Monad      (forM_, forever, unless, when)
import           Control.Monad.Cont (callCC, lift, runContT)
import           Data.Int           (Int64)
import           Data.List          (sortOn)
import           Metro.Utils        (getEpochTime)
import           Periodic.Types     (Msgid, Nid)
import           Periodic.Types.Job
import           UnliftIO           hiding (poll)

data SchedState = SchedState
  { stateIsBusy :: Bool
  , stateDelay  :: TVar Bool  -- when delay true job is sched
  , stateJob    :: Maybe Job
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
  , quickRunJob :: TVar [Job]
  , waitingJob  :: TVar [Job]
  , sortedFlag  :: TVar Bool
  , maxPoolSize :: TVar Int
  , poolSize    :: TVar Int
  , currentSize :: TVar Int
  , onFree      :: STM ()
  , prepareWork :: FuncName -> STM [(Nid, Msgid)]
  }


newSchedPool
  :: MonadIO m
  => TVar Int
  -> STM ()
  -> (FuncName -> STM [(Nid, Msgid)])
  -> m SchedPool
newSchedPool maxPoolSize onFree prepareWork = do
  poolerList  <- newTVarIO []
  lastState   <- newTVarIO Nothing
  freeStates  <- newTVarIO []
  quickRunJob <- newTVarIO []
  waitingJob  <- newTVarIO []
  sortedFlag  <- newTVarIO True
  poolSize    <- newTVarIO 0
  currentSize <- newTVarIO 0
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
            Just j ->
              if getHandle j == jh then do
                delay <- readTVar $ stateDelay state
                if delay then pure Nothing
                         else pure $ Just $ poolerState x
              else go xs

        jh = getHandle job


getHandleList :: MonadIO m => SchedPool -> m [JobHandle]
getHandleList SchedPool {..} =
  atomically $ do
    handles0 <- readTVar poolerList >>= go
    handles1 <- map getHandle <$> readTVar quickRunJob
    handles2 <- map getHandle <$> readTVar waitingJob
    pure $ handles0 ++ handles1 ++ handles2
  where go :: [SchedPooler] -> STM [JobHandle]
        go [] = pure []
        go (x:xs) = do
          state <- readTVar $ poolerState x
          case stateJob state of
            Nothing -> go xs
            Just j -> do
              delay <- readTVar $ stateDelay state
              if delay then go xs
                       else do
                         handles <- go xs
                         pure $ getHandle j : handles


getFreeState :: MonadIO m => FreeStates -> m (Maybe PoolerState)
getFreeState freeStates = atomically $ do
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
          delay <- stateDelay <$> readTVar (poolerState pooler)
          st <- readTVar delay
          if st then pure Nothing
                else pure $ Just pooler
        comp (Just pooler0) pooler1 = do
          s0 <- getPoolerSchedAt pooler0
          s1 <- getPoolerSchedAt pooler1
          if s0 > s1 then pure $ Just pooler0
                     else pure $ Just pooler1

getPoolerSchedAt :: SchedPooler -> STM Int64
getPoolerSchedAt pooler =
  maybe 0 getSchedAt . stateJob <$> readTVar (poolerState pooler)


getLastStateSchedAt :: LastState -> STM Int64
getLastStateSchedAt ls = do
  mState <- readTVar ls
  case mState of
    Nothing    -> pure 0
    Just state -> maybe 0 getSchedAt . stateJob <$> readTVar state


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
  -> (Job -> Bool -> (Nid, Msgid) -> m ())
  -> PoolerState -> m (Async ())
startPoolerIO pool@SchedPool {..} work state =
  async $ (`runContT` pure) $ callCC $ \exit -> forever $ do
    (job, agents) <- atomically $ do
      v <- readTVar state
      case stateJob v of
        Nothing  -> retrySTM
        Just job -> do
          canDo <- readTVar $ stateDelay v
          if canDo then do
            agents <- prepareWork $ getFuncName job
            pure (job, agents)
          else retrySTM

    mapM_ (lift . work job (length agents == 1)) agents

    s <- readTVarIO state

    if stateAlive s then finishPoolerState pool state
                    else exit ()


finishPoolerState :: MonadIO m => SchedPool -> PoolerState -> m ()
finishPoolerState SchedPool {..} state =
  atomically $ do
    jobs <- readTVar quickRunJob
    case jobs of
      [] -> do
        modifyTVar' state $ \v -> v
          { stateJob   = Nothing
          , stateIsBusy = False
          }
        modifyTVar' currentSize (\v -> v - 1)
        modifyTVar' freeStates (state:)
        onFree
      (x:xs) -> do
        writeTVar quickRunJob xs
        modifyTVar' state $ \v -> v
          { stateJob = Just x
          }


unspawn :: MonadIO m => SchedPool -> Job -> m ()
unspawn pool job = findPoolerState pool job >>= mapM_ (finishPoolerState pool)


spawn
  :: MonadUnliftIO m
  => SchedPool
  -> (Job -> Bool -> (Nid, Msgid) -> m ())
  -> Job -> m ()
spawn pool@SchedPool {..} work job = do
  now <- getEpochTime
  mFreeState <- getFreeState freeStates
  case mFreeState of
    Just state -> do
      delay <- getDelay now
      atomically $ do
        modifyTVar' state $ genPoolState delay
        modifyTVar' currentSize (+1)
        trySwapLastState lastState state schedAt
    Nothing -> do
      size <- readTVarIO poolSize
      maxSize <- readTVarIO maxPoolSize
      when (maxSize < size) $ atomically $ do
        poolers <- readTVar poolerList
        writeTVar poolerList $! drop (size - maxSize) poolers
        forM_ (take (size - maxSize) poolers) $ \p ->
          modifyTVar' (poolerState p) $ \st -> st {stateAlive = False}

      if size < maxSize then do
        delay <- getDelay now

        state <- newTVarIO SchedState
          { stateIsBusy = True
          , stateJob    = Just job
          , stateDelay  = delay
          , stateAlive  = True
          }

        io <- startPoolerIO pool work state

        atomically $ do
          modifyTVar' poolerList (++[SchedPooler state io])
          modifyTVar' poolSize (+1)
          modifyTVar' currentSize (+1)
          trySwapLastState lastState state schedAt
      else do
        mState <- atomically $ do
          lastSchedAt <- getLastStateSchedAt lastState
          if lastSchedAt > schedAt then do
            delay <- newTVar False
            mLastState <- readTVar lastState
            case mLastState of
              Nothing -> pure Nothing
              Just state -> do
                mJob <- stateJob <$> readTVar state
                case mJob of
                  Nothing   -> pure ()
                  Just oJob -> do
                    modifyTVar' waitingJob (oJob:)
                    writeTVar sortedFlag False

                modifyTVar' state $ genPoolState delay
                mLastPooler <- getLastPooler poolerList
                case mLastPooler of
                  Nothing -> pure $ Just state
                  Just pooler -> do
                    swapLastState lastState (poolerState pooler)
                    pure $ Just state
          else do
            if schedAt <= now + 1 then modifyTVar' quickRunJob (job:)
                                  else do
                                    modifyTVar' waitingJob (job:)
                                    writeTVar sortedFlag False

            pure Nothing
        case mState of
          Nothing -> pure ()
          Just state -> do
            delay <- getDelay now
            atomically $ modifyTVar' state $ \v -> v {stateDelay = delay}

  where schedAt = getSchedAt job
        genPoolState delay state = state
          { stateIsBusy = True
          , stateJob = Just job
          , stateDelay = delay
          }

        getDelay :: MonadIO m => Int64 -> m (TVar Bool)
        getDelay now
          | schedAt > now + 1 = registerDelay delayUS
          | otherwise         = newTVarIO True
          where delayUS = fromIntegral $ (schedAt - now) * 1000000


poll :: MonadIO m => SchedPool -> (Job -> m ()) -> (Int -> m ()) -> m ()
poll SchedPool {..} respawn trySpawn = do
  now <- getEpochTime
  jobs <- atomically $ do
    isSorted <- readTVar sortedFlag
    unless isSorted $ modifyTVar' waitingJob $ sortOn getSchedAt
    wJobs <- readTVar waitingJob

    case takeWhile (\x -> getSchedAt x <= now) wJobs of
      [] -> do
        case wJobs of
          [] -> pure []
          (x:xs) -> do
            writeTVar waitingJob xs
            pure [x]
      xs -> do
        writeTVar waitingJob $! dropWhile (\x -> getSchedAt x <= now) wJobs
        pure xs

  mapM_ respawn jobs

  when (null jobs) $ readTVarIO currentSize >>= trySpawn


close :: MonadIO m => SchedPool -> m ()
close SchedPool {..} = readTVarIO poolerList >>= mapM_ (cancel . poolerIO)
