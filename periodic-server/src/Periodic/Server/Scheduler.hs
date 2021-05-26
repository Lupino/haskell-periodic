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

import           Control.Monad                (forever, unless, void, when)
import           Control.Monad.Cont           (callCC, runContT)
import           Control.Monad.Reader.Class   (MonadReader (ask), asks)
import           Control.Monad.Trans.Class    (MonadTrans, lift)
import           Control.Monad.Trans.Reader   (ReaderT (..), runReaderT)
import           Data.ByteString              (ByteString)
import           Data.Foldable                (forM_)
import           Data.IOMap                   (IOMap)
import qualified Data.IOMap                   as IOMap
import qualified Data.IOMap.STM               as IOMapS
import           Data.Int                     (Int64)
import           Data.IntMap.Strict           (IntMap)
import qualified Data.IntMap.Strict           as IntMap
import qualified Data.List                    as L (delete)
import           Data.Maybe                   (fromJust, fromMaybe, isJust)
import           Metro.Class                  (Transport)
import qualified Metro.Lock                   as L (Lock, new, with)
import           Metro.Session                (runSessionT1, send, sessionState)
import           Metro.Utils                  (getEpochTime)
import           Periodic.Server.FuncStat
import           Periodic.Server.GrabQueue
import           Periodic.Server.Persist      (Persist, State (..))
import qualified Periodic.Server.Persist      as P
import           Periodic.Server.Types        (CSEnv)
import           Periodic.Types               (packetRES)
import           Periodic.Types.Internal      (LockName)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand (ServerCommand (JobAssign))
import           System.Log.Logger            (errorM, infoM)
import           UnliftIO
import           UnliftIO.Concurrent          (threadDelay)

data Action = Add Job
    | Remove Job
    | Cancel
    | PollJob
    | TryPoll JobHandle

data WaitItem = WaitItem
    { itemTs    :: Int64
    , itemValue :: Maybe ByteString
    , itemWait  :: Int
    }

-- Cache runJob result
--                                   expiredAt, Nothing       retrySTM
--                                   expiredAt, Just bs       return bs
type WaitList = IOMap JobHandle WaitItem

-- Distributed lock
--                                  acquired    locked
data LockInfo = LockInfo
    { acquired :: [JobHandle]
    , locked   :: [JobHandle]
    , maxCount :: Int
    }

type LockList = IOMap LockName LockInfo

data SchedEnv db tp = SchedEnv
    { sPollInterval   :: TVar Int -- main poll loop every time interval
    -- revert process queue loop every time interval
    , sRevertInterval :: TVar Int -- revert process queue loop every time interval
    -- the task do timeout
    , sTaskTimeout    :: TVar Int -- the task do timeout
    -- max poll batch size
    , sMaxBatchSize   :: TVar Int -- max poll batch size
    -- client or worker keepalive
    , sKeepalive      :: TVar Int -- client or worker keepalive
    -- run job cache expiration
    , sExpiration     :: TVar Int -- run job cache expiration
    -- auto poll job when job done or failed
    , sAutoPoll       :: TVar Bool -- auto poll job when job done or failed
    -- auto poll lock
    , sPolled         :: TVar Bool -- auto poll lock
    , sCleanup        :: IO ()
    , sFuncStatList   :: FuncStatList
    , sLocker         :: L.Lock
    , sGrabQueue      :: GrabQueue tp
    -- sched state, when false sched is exited.
    , sAlive          :: TVar Bool -- sched state, when false sched is exited.
    , sChanList       :: TVar [Action]
    , sWaitList       :: WaitList
    , sLockList       :: LockList
    , sPersist        :: db
    }

newtype SchedT db tp m a = SchedT {unSchedT :: ReaderT (SchedEnv db tp) m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , MonadReader (SchedEnv db tp)
    )

instance MonadUnliftIO m => MonadUnliftIO (SchedT db tp m) where
  withRunInIO inner = SchedT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runSchedT r)


type TaskList = IOMap JobHandle (Int64, Async ())

runSchedT :: SchedEnv db tp -> SchedT db tp m a -> m a
runSchedT schedEnv = flip runReaderT schedEnv . unSchedT

initSchedEnv :: (MonadUnliftIO m, Persist db) => P.PersistConfig db -> m () -> m (SchedEnv db tp)
initSchedEnv config sC = do
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
  sMaxBatchSize   <- newTVarIO 250
  sKeepalive      <- newTVarIO 300
  sExpiration     <- newTVarIO 300
  sAutoPoll       <- newTVarIO False
  sPolled         <- newTVarIO False
  sCleanup        <- toIO sC
  sPersist        <- liftIO $ P.newPersist config
  pure SchedEnv{..}

startSchedT :: (MonadUnliftIO m, Persist db, Transport tp) => SchedT db tp m ()
startSchedT = do
  liftIO $ infoM "Periodic.Server.Scheduler" "Scheduler started"
  SchedEnv{..} <- ask
  runTask_ sRevertInterval revertRunningQueue
  taskList <- IOMap.empty
  runTask_ sPollInterval $ pushChanList PollJob
  runTask 0 $ runChanJob taskList
  runTask 100 purgeExpired
  runTask 60 revertLockingQueue

  loadInt "poll-interval" sPollInterval
  loadInt "revert-interval" sRevertInterval
  loadInt "timeout" sTaskTimeout
  loadInt "keepalive" sKeepalive
  loadInt "max-batch-size" sMaxBatchSize
  loadInt "expiration" sExpiration

loadInt :: (MonadIO m, Persist db) => String -> TVar Int -> SchedT db tp m ()
loadInt name ref = do
  v <- liftIO . flip P.configGet name =<< asks sPersist
  case v of
    Nothing -> pure ()
    Just v' -> atomically $ writeTVar ref v'

saveInt :: (MonadIO m, Persist db) => String -> Int -> TVar Int -> SchedT db tp m ()
saveInt name v ref = do
  p <- asks sPersist
  liftIO $ P.configSet p name v
  atomically $ writeTVar ref v

setConfigInt :: (MonadIO m, Persist db) => String -> Int -> SchedT db tp m ()
setConfigInt key val = do
  SchedEnv {..} <- ask
  case key of
    "poll-interval"   -> saveInt "poll-interval" val sPollInterval
    "revert-interval" -> saveInt "revert-interval" val sRevertInterval
    "timeout"         -> saveInt "timeout" val sTaskTimeout
    "keepalive"       -> saveInt "keepalive" val sKeepalive
    "max-batch-size"  -> saveInt "max-batch-size" val sMaxBatchSize
    "expiration"      -> saveInt "expiration" val sExpiration
    _                 -> pure ()

getConfigInt :: (MonadIO m, Persist db) => String -> SchedT db tp m Int
getConfigInt key = do
  SchedEnv {..} <- ask
  case key of
    "poll-interval"   -> readTVarIO sPollInterval
    "revert-interval" -> readTVarIO sRevertInterval
    "timeout"         -> readTVarIO sTaskTimeout
    "keepalive"       -> readTVarIO sKeepalive
    "max-batch-size"  -> readTVarIO sMaxBatchSize
    "expiration"      -> readTVarIO sExpiration
    _                 -> pure 0

keepalive :: Monad m => SchedT db tp m (TVar Int)
keepalive = asks sKeepalive

runTask :: (MonadUnliftIO m) => Int -> SchedT db tp m () -> SchedT db tp m ()
runTask d m = flip runTask_ m =<< newTVarIO d

runTask_ :: (MonadUnliftIO m) => TVar Int -> SchedT db tp m () -> SchedT db tp m ()
runTask_ d m = void . async $ do
  SchedEnv{..} <- ask
  (`runContT` pure) $ callCC $ \exit -> forever $ do
    interval <- readTVarIO d
    when (interval > 0) $ threadDelay $ interval * 1000 * 1000
    alive <- readTVarIO sAlive
    if alive then lift m
             else exit ()

runChanJob
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => TaskList -> SchedT db tp m ()
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

  mapM_ (doChanJob taskList) acts

  where doChanJob
          :: (MonadUnliftIO m, Persist db, Transport tp)
          => TaskList -> Action -> SchedT db tp m ()
        doChanJob tl (Add job)    = reSchedJob tl job
        doChanJob tl (Remove job) = findTask tl job >>= mapM_ cancel
        doChanJob tl Cancel       = mapM_ (cancel . snd) =<< IOMap.elems tl
        doChanJob tl PollJob      = pollJob tl
        doChanJob tl (TryPoll jh) = removeTaskAndTryPoll tl jh


pollInterval :: (MonadIO m, Num a) => SchedT db tp m a
pollInterval = fmap fromIntegral . readTVarIO =<< asks sPollInterval

removeTaskAndTryPoll :: MonadIO m => TaskList -> JobHandle -> SchedT db tp m ()
removeTaskAndTryPoll taskList jh = do
  IOMap.delete jh taskList
  polled <- asks sPolled
  isPolled <- readTVarIO polled
  autoPoll <- readTVarIO =<< asks sAutoPoll
  when (isPolled && autoPoll) $ do
    maxBatchSize <- readTVarIO =<< asks sMaxBatchSize
    size <- IOMap.size taskList
    when (size < maxBatchSize) $ do
      atomically $ writeTVar polled False
      pushChanList PollJob

pollJob
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => TaskList -> SchedT db tp m ()
pollJob taskList = do
  polled <- asks sPolled
  atomically $ writeTVar polled False
  mapM_ checkPoll =<< IOMap.toList taskList
  stList <- asks sFuncStatList
  funcList <- foldr foldFunc [] <$> IOMap.toList stList
  pollJob_ taskList funcList
  atomically $ writeTVar polled True

  where foldFunc :: (FuncName, FuncStat) -> [FuncName] -> [FuncName]
        foldFunc (_, FuncStat{sWorker=0}) acc = acc
        foldFunc (fn, _) acc                  = fn:acc

        checkPoll
          :: (MonadIO m)
          => (JobHandle, (Int64, Async ())) -> SchedT db tp m ()
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
              unless r0 $ cancel w

          where (fn, _) = unHandle jh

pollJob_
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => TaskList -> [FuncName] -> SchedT db tp m ()
pollJob_ _ [] = pure ()
pollJob_ taskList funcList = do
  now <- getEpochTime
  next <- (+ (100 + now)) <$> pollInterval
  handles <- IOMap.keys taskList
  let check job = notElem (getHandle job) handles && (getSchedAt job < next)

  maxBatchSize <- readTVarIO =<< asks sMaxBatchSize
  p <- asks sPersist
  jobs <- liftIO $
    P.foldrPending p next funcList (foldFunc (maxBatchSize * 2) check now) IntMap.empty

  mapM_ (checkJob taskList) jobs

  autoPoll <- asks sAutoPoll
  atomically $ writeTVar autoPoll (length jobs > maxBatchSize)

  where foldFunc :: Int -> (Job -> Bool) -> Int64 -> Job -> IntMap Job -> IntMap Job
        foldFunc s f now job acc | f job = trimPSQ $ IntMap.insert (fromIntegral (now - getSchedAt job)) job acc
                                 | otherwise = acc
          where trimPSQ ::  IntMap Job -> IntMap Job
                trimPSQ q | IntMap.size q > s = trimPSQ $ IntMap.deleteMin q
                          | otherwise = q

        checkJob
          :: (MonadUnliftIO m, Persist db, Transport tp)
          => TaskList -> Job -> SchedT db tp m ()
        checkJob tl job = do
          w <- findTask tl job
          case w of
            Nothing -> do
              p <- asks sPersist
              isProc <- liftIO $ P.member p Running fn jn
              unless isProc $ reSchedJob tl job
            Just w0 -> do
              r <- canRun fn
              unless r $ cancel w0
          where fn = getFuncName job
                jn = getName job

pushChanList :: MonadIO m => Action -> SchedT db tp m ()
pushChanList act = do
  cl <- asks sChanList
  atomically $ do
    l <- readTVar cl
    writeTVar cl (act:l)

pushJob :: (MonadIO m, Persist db) => Job -> SchedT db tp m ()
pushJob job = do
  liftIO $ infoM "Periodic.Server.Scheduler" ("pushJob: " ++ show (getHandle job))
  p <- asks sPersist
  isRunning <- liftIO $ P.member p Running fn jn
  unless isRunning $ do
    job' <- fixedSchedAt job
    liftIO $ P.insert p Pending fn jn job'
    pushChanList (Add job')

  where fn = getFuncName job
        jn = getName job


fixedSchedAt :: MonadIO m => Job -> SchedT db tp m Job
fixedSchedAt job = do
  now <- getEpochTime
  if getSchedAt job < now then
    return $ setSchedAt now job
  else return job

reSchedJob :: (MonadUnliftIO m, Persist db, Transport tp) => TaskList -> Job -> SchedT db tp m ()
reSchedJob taskList job = do
  w <- findTask taskList job
  forM_ w cancel

  interval <- (+100) <$> pollInterval
  next <- (+ interval) <$> getEpochTime
  when (getSchedAt job < next) $ do
    r <- canRun $ getFuncName job
    c <- check taskList
    when (r && c) $ do
      w' <- schedJob taskList job
      IOMap.insert (getHandle job) (getSchedAt job, w') taskList
  where check :: (MonadIO m) => TaskList -> SchedT db tp m Bool
        check tl = do
          maxBatchSize <- readTVarIO =<< asks sMaxBatchSize
          size <- IOMap.size tl
          if size < maxBatchSize * 2 then return True
          else do
            lastTask <- findLastTask tl
            case lastTask of
              Nothing -> return True
              Just (sc, jh, w) ->
                if sc < getSchedAt job then return False
                else do
                  cancel w
                  IOMap.delete jh taskList
                  return True

findTask :: MonadIO m => TaskList -> Job -> SchedT db tp m (Maybe (Async ()))
findTask taskList job = fmap snd <$> IOMap.lookup (getHandle job) taskList

findLastTask :: MonadIO m => TaskList -> SchedT db tp m (Maybe (Int64, JobHandle, Async ()))
findLastTask = IOMap.foldrWithKey f Nothing
  where f :: JobHandle
          -> (Int64, a)
          -> Maybe (Int64, JobHandle, a)
          -> Maybe (Int64, JobHandle, a)
        f jh (sc, t) Nothing = Just (sc, jh, t)
        f jh (sc, t) (Just (sc1, jh1, t1))
          | sc > sc1 = Just (sc, jh, t)
          | otherwise = Just (sc1, jh1, t1)

canRun :: MonadIO m => FuncName -> SchedT db tp m Bool
canRun fn = asks sFuncStatList >>= flip canRun_ fn

canRun_ :: MonadIO m => FuncStatList -> FuncName -> m Bool
canRun_ stList fn = do
  st0 <- IOMap.lookup fn stList
  case st0 of
    Nothing                  -> pure False
    Just FuncStat{sWorker=0} -> pure False
    Just _                   -> pure True

schedJob
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => TaskList -> Job -> SchedT db tp m (Async ())
schedJob taskList = async . schedJob_ taskList

schedJob_ :: (MonadUnliftIO m, Persist db, Transport tp) => TaskList -> Job -> SchedT db tp m ()
schedJob_ taskList job = do
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
                  else popAgentThen taskList

  where fn = getFuncName job
        jn = getName job
        schedAt = getSchedAt job
        jh = getHandle job

        popAgentThen
          :: (MonadUnliftIO m, Persist db, Transport tp) => TaskList -> SchedT db tp m ()
        popAgentThen tl = do
          SchedEnv{..} <- ask
          (jq, env0) <- atomically $ popAgentSTM sGrabQueue fn
          alive <- runSessionT1 env0 sessionState
          if alive then do
            IOMap.insert (getHandle job) () jq
            nextSchedAt <- getEpochTime
            liftIO $ P.insert sPersist Running fn jn $ setSchedAt nextSchedAt job
            r <- doSubmitJob env0
            case r of
              Left _ -> do
                liftIO $ P.insert sPersist Pending fn jn $ setSchedAt nextSchedAt job
                IOMap.delete jh jq
                schedJob_ tl job
              Right _ -> endSchedJob
          else schedJob_ tl job

        popAgentListThen :: (MonadUnliftIO m, Transport tp) => SchedT db tp m ()
        popAgentListThen = do
          SchedEnv{..} <- ask
          agents <- popAgentList sGrabQueue fn
          mapM_ (doSubmitJob . snd) agents
          unless (null agents) endSchedJob -- wait to resched the broadcast job

        doSubmitJob :: (MonadUnliftIO m, Transport tp) => CSEnv tp -> SchedT db tp m (Either SomeException ())
        doSubmitJob agent = do
          SchedEnv{..} <- ask
          tryAny $ assignJob agent job

        endSchedJob :: MonadIO m => SchedT db tp m ()
        endSchedJob = pushChanList (TryPoll jh)

adjustFuncStat :: (MonadIO m, Persist db) => FuncName -> SchedT db tp m ()
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

removeJob :: (MonadIO m, Persist db) => Job -> SchedT db tp m ()
removeJob job = do
  liftIO $ infoM "Periodic.Server.Scheduler" ("removeJob: " ++ show (getHandle job))
  p <- asks sPersist
  liftIO $ P.delete p fn jn

  pushChanList (Remove job)
  pushResult jh ""
  where jn = getName job
        fn = getFuncName job
        jh = getHandle job

dumpJob :: (MonadIO m, Persist db) => SchedT db tp m [Job]
dumpJob = liftIO . P.dumpJob =<< asks sPersist

alterFunc :: (MonadIO m, Persist db) => FuncName -> (Maybe FuncStat -> Maybe FuncStat) -> SchedT db tp m ()
alterFunc n f = do
  SchedEnv{..} <- ask
  IOMap.alter f n sFuncStatList
  liftIO $ P.insertFuncName sPersist n
  pushChanList PollJob

addFunc :: (MonadIO m, Persist db) => FuncName -> SchedT db tp m ()
addFunc n = broadcastFunc n False

broadcastFunc :: (MonadIO m, Persist db) => FuncName -> Bool -> SchedT db tp m ()
broadcastFunc n cast = do
  liftIO $ infoM "Periodic.Server.Scheduler" (h ++ ": " ++ show n)
  alterFunc n updateStat

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just ((funcStat n) {sWorker = 1, sBroadcast = cast})
        updateStat (Just fs) = Just (fs { sWorker = sWorker fs + 1, sBroadcast = cast })

        h = if cast then "broadcastFunc" else "addFunc"

removeFunc :: (MonadIO m, Persist db) => FuncName -> SchedT db tp m ()
removeFunc n = do
  liftIO $ infoM "Periodic.Server.Scheduler" ("removeFunc: " ++ show n)
  alterFunc n updateStat

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just (funcStat n)
        updateStat (Just fs) = Just (fs { sWorker = max (sWorker fs - 1) 0 })

dropFunc :: (MonadUnliftIO m, Persist db) => FuncName -> SchedT db tp m ()
dropFunc n = do
  liftIO $ infoM "Periodic.Server.Scheduler" ("dropFunc: " ++ show n)
  SchedEnv{..} <- ask
  L.with sLocker $ do
    st <- IOMap.lookup n sFuncStatList
    case st of
      Just FuncStat{sWorker=0} -> do
        IOMap.delete n sFuncStatList
        liftIO $ P.removeFuncName sPersist n
      _                        -> pure ()

  pushChanList PollJob

pushGrab :: MonadIO m => IOMap FuncName () -> IOMap JobHandle () -> CSEnv tp -> SchedT db tp m ()
pushGrab funcList handleList ag = do
  queue <- asks sGrabQueue
  pushAgent queue funcList handleList ag

assignJob :: (MonadUnliftIO m, Transport tp) => CSEnv tp -> Job -> m ()
assignJob env0 job = do
  liftIO $ infoM "Periodic.Server.Scheduler" ("assignJob: " ++ show (getHandle job))
  runSessionT1 env0 $ send $ packetRES (JobAssign job)

failJob :: (MonadUnliftIO m, Persist db) => JobHandle -> SchedT db tp m ()
failJob jh = do
  liftIO $ infoM "Periodic.Server.Scheduler" ("failJob: " ++ show jh)
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

retryJob :: (MonadIO m, Persist db) => Job -> SchedT db tp m ()
retryJob job = do
  p <- asks sPersist
  liftIO $ P.insert p Pending fn jn job

  pushChanList (Add job)

  where  fn = getFuncName job
         jn = getName job

doneJob
  :: (MonadUnliftIO m, Persist db)
  => JobHandle -> ByteString -> SchedT db tp m ()
doneJob jh w = do
  liftIO $ infoM "Periodic.Server.Scheduler" ("doneJob: " ++ show jh)
  releaseLock' jh
  p <- asks sPersist
  liftIO $ P.delete p fn jn
  pushResult jh w
  where (fn, jn) = unHandle jh

schedLaterJob
  :: (MonadUnliftIO m, Persist db)
  => JobHandle -> Int64 -> Int -> SchedT db tp m ()
schedLaterJob jh later step = do
  liftIO $ infoM "Periodic.Server.Scheduler" ("schedLaterJob: " ++ show jh)
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

      nextSchedAt <- (+) later <$> getEpochTime
      retryJob $ setCount (getCount job' + step) $ setSchedAt nextSchedAt job'

  where (fn, jn) = unHandle jh

acquireLock
  :: (MonadUnliftIO m, Persist db)
  => LockName -> Int -> JobHandle -> SchedT db tp m Bool
acquireLock name count jh = do
  liftIO $ infoM "Periodic.Server.Scheduler" ("acquireLock: " ++ show name ++ " " ++ show count ++ " " ++ show jh)
  locker <- asks sLocker
  L.with locker $ do
    lockList <- asks sLockList
    p <- asks sPersist
    j <- liftIO $ P.lookup p Running fn jn
    case j of
      Nothing -> pure True
      Just job -> do
        r <- atomically $ do
          l <- IOMapS.lookup name lockList
          case l of
            Nothing -> do
              IOMapS.insert name LockInfo
                { acquired = [jh]
                , locked = []
                , maxCount = count
                } lockList
              pure True
            Just info@LockInfo {..} -> do
              let newCount = max maxCount count
              if jh `elem` acquired then pure True
              else if jh `elem` locked then pure False
              else
                if length acquired < maxCount then do
                  IOMapS.insert name info
                    { acquired = acquired ++ [jh]
                    , maxCount = newCount
                    } lockList
                  pure True
                else do
                  IOMapS.insert name info
                    { locked = locked ++ [jh]
                    , maxCount = newCount
                    } lockList
                  pure False

        unless r $ liftIO $ P.insert p Locking fn jn job
        return r

  where (fn, jn) = unHandle jh

releaseLock
  :: (MonadUnliftIO m, Persist db)
  => LockName -> JobHandle -> SchedT db tp m ()
releaseLock name jh = do
  locker <- asks sLocker
  L.with locker $ releaseLock_ name jh

releaseLock_
  :: (MonadUnliftIO m, Persist db)
  => LockName -> JobHandle -> SchedT db tp m ()
releaseLock_ name jh = do
  liftIO $ infoM "Periodic.Server.Scheduler" ("releaseLock: " ++ show name ++ " " ++ show jh)
  p <- asks sPersist
  lockList <- asks sLockList
  h <- atomically $ do
    l <- IOMapS.lookup name lockList
    case l of
      Nothing -> pure Nothing
      Just info@LockInfo {..} ->
        if jh `elem` acquired then
          case locked of
            [] -> do
              IOMapS.insert name info
                { acquired = L.delete jh acquired
                } lockList
              pure Nothing
            x:xs -> do
              IOMapS.insert name info
                { acquired = L.delete jh acquired
                , locked   = xs
                } lockList
              pure $ Just x
        else pure Nothing

  case h of
    Nothing -> pure ()
    Just hh -> do
      let (fn, jn) = unHandle hh
      j <- liftIO $ P.lookup p Locking fn jn
      case j of
        Nothing  -> releaseLock_ name hh
        Just job -> do
          liftIO $ P.insert p Pending fn jn job
          pushChanList (Add job)

releaseLock'
  :: (MonadUnliftIO m, Persist db)
  => JobHandle -> SchedT db tp m ()
releaseLock' jh = do
  lockList <- asks sLockList
  names <- IOMap.foldrWithKey foldFunc [] lockList
  mapM_ (`releaseLock` jh) names

  where foldFunc :: LockName -> LockInfo -> [LockName] -> [LockName]
        foldFunc n LockInfo {..} acc | jh `elem` acquired = n : acc
                                     | jh `elem` locked   = n : acc
                                     | otherwise          = acc

countLock :: MonadUnliftIO m => (LockInfo -> [JobHandle]) -> FuncName -> SchedT db tp m Int
countLock f fn = do
  lockList <- asks sLockList
  sum . map mapFunc <$> IOMap.elems lockList
  where filterFunc :: JobHandle -> Bool
        filterFunc jh = fn0 == fn
          where (fn0, _) = unHandle jh

        mapFunc :: LockInfo -> Int
        mapFunc = length . filter filterFunc . f

getMaxLockCount :: MonadUnliftIO m => SchedT db tp m Int
getMaxLockCount = do
  lockList <- asks sLockList
  maximum . map maxCount <$> IOMap.elems lockList


status :: (MonadIO m, Persist db) => SchedT db tp m [FuncStat]
status = do
  mapM_ adjustFuncStat =<< liftIO . P.funcList =<< asks sPersist
  IOMap.elems =<< asks sFuncStatList

revertRunningQueue :: (MonadUnliftIO m, Persist db) => SchedT db tp m ()
revertRunningQueue = do
  now <- getEpochTime
  tout <- fmap fromIntegral . readTVarIO =<< asks sTaskTimeout
  p <- asks sPersist
  handles <- liftIO $ P.foldr p Running (foldFunc (check now tout)) []
  mapM_ (failJob . getHandle) handles

  where foldFunc :: (Job -> Bool) -> Job -> [Job] -> [Job]
        foldFunc f job acc | f job = job : acc
                           | otherwise = acc

        check :: Int64 -> Int64 -> Job -> Bool
        check now t0 job
          | getTimeout job > 0 = getSchedAt job + fromIntegral (getTimeout job) < now
          | otherwise = getSchedAt job + fromIntegral t0 < now

revertLockingQueue :: (MonadUnliftIO m, Persist db) => SchedT db tp m ()
revertLockingQueue = mapM_ checkAndReleaseLock =<< liftIO . P.funcList =<< asks sPersist

  where checkAndReleaseLock
          :: (MonadUnliftIO m, Persist db)
          => FuncName -> SchedT db tp m ()
        checkAndReleaseLock fn = do
          p <- asks sPersist
          sizeLocked <- countLock locked fn
          sizeAcquired <- countLock acquired fn
          liftIO $ infoM "Peridic.Server.Scheduler"
                 $ "LockInfo " ++ show fn
                 ++ " Locked:" ++ show sizeLocked
                 ++ " Acquired:" ++ show sizeAcquired
          when (sizeLocked > 0 && sizeAcquired == 0) $ do
            count <- getMaxLockCount
            handles <- liftIO $ P.foldrLocking p count fn (:) []
            mapM_ pushJob handles


purgeExpired :: MonadIO m => SchedT db tp m ()
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

shutdown :: (MonadUnliftIO m) => SchedT db tp m ()
shutdown = do
  liftIO $ infoM "Periodic.Server.Scheduler" "Scheduler shutdown"
  SchedEnv{..} <- ask
  pushChanList Cancel
  alive <- atomically $ do
    t <- readTVar sAlive
    writeTVar sAlive False
    return t
  when alive . void . async $ liftIO sCleanup

prepareWait :: MonadIO m => Job -> SchedT db tp m ()
prepareWait job = pushResult_ updateWL jh
  where updateWL :: Int64 -> Maybe WaitItem -> Maybe WaitItem
        updateWL now Nothing       = Just $ WaitItem {itemTs = now, itemValue = Nothing, itemWait = 1}
        updateWL now (Just item) = Just $ item {itemTs = now, itemWait = itemWait item + 1}

        jh = getHandle job

waitResult :: MonadIO m => TVar Bool -> Job -> SchedT db tp m ByteString
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
  => JobHandle -> ByteString -> SchedT db tp m ()
pushResult jh w = pushResult_ updateWL jh
  where updateWL :: Int64 -> Maybe WaitItem -> Maybe WaitItem
        updateWL _ Nothing       = Nothing
        updateWL now (Just item) = Just item {itemTs=now, itemValue = Just w}

pushResult_
  :: MonadIO m
  => (Int64 -> Maybe WaitItem -> Maybe WaitItem)
  -> JobHandle -> SchedT db tp m ()
pushResult_ f jh = do
  wl <- asks sWaitList
  now <- getEpochTime
  IOMap.alter (f now) jh wl

existsWaitList :: MonadIO m => JobHandle -> SchedT db tp m Bool
existsWaitList jh = do
  wl <- asks sWaitList
  isJust <$> IOMap.lookup jh wl

lookupPrevResult :: MonadIO m => Job -> SchedT db tp m (Maybe ByteString)
lookupPrevResult job = do
  wl <- asks sWaitList
  r <- IOMap.lookup jh wl
  case r of
    Nothing                               -> pure Nothing
    (Just WaitItem {itemValue = Nothing}) -> pure Nothing
    (Just WaitItem {itemValue = Just v})  -> pure (Just v)

  where jh = getHandle job

removeFromWaitList :: MonadIO m => JobHandle -> SchedT db tp m ()
removeFromWaitList jh = do
  wl <- asks sWaitList
  IOMap.delete jh wl
