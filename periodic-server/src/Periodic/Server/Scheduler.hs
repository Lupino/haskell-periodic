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
  , waitResult
  ) where

import           Control.Monad                (forever, mzero, unless, void,
                                               when)
import           Control.Monad.Reader.Class   (MonadReader (ask), asks)
import           Control.Monad.Trans.Class    (MonadTrans, lift)
import           Control.Monad.Trans.Maybe    (runMaybeT)
import           Control.Monad.Trans.Reader   (ReaderT (..), runReaderT)
import           Data.ByteString              (ByteString)
import           Data.Foldable                (forM_)
import           Data.HashPSQ                 (HashPSQ)
import qualified Data.HashPSQ                 as PSQ
import           Data.Int                     (Int64)
import qualified Data.List                    as L (delete, nub, uncons)
import           Data.Maybe                   (fromJust, fromMaybe, isJust)
import           Periodic.Agent               (AgentEnv', aAlive, runAgentT',
                                               send)
import           Periodic.IOHashMap           (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap           as FL
import           Periodic.IOList              (IOList)
import qualified Periodic.IOList              as IL
import qualified Periodic.Lock                as L (Lock, new, with)
import           Periodic.Server.FuncStat
import           Periodic.Server.GrabQueue
import           Periodic.Server.Persist      (Persist, State (..))
import qualified Periodic.Server.Persist      as P
import           Periodic.Types.Internal      (LockName)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand (ServerCommand (JobAssign))
import           Periodic.Utils               (getEpochTime)
import           System.Log.Logger            (errorM)
import           UnliftIO
import           UnliftIO.Concurrent          (threadDelay)

data Action = Add Job | Remove Job | Cancel | PollJob | TryPoll JobHandle

type WaitList = IOHashMap JobHandle (Int64, Maybe ByteString)

type LockList = IOHashMap LockName ([JobHandle], [JobHandle])

data SchedEnv = SchedEnv
  { sPollDelay    :: TVar Int -- main poll loop every time delay
  , sRevertDelay  :: TVar Int -- revert process queue loop every time delay
  , sTaskTimeout  :: TVar Int -- the task do timeout
  , sMaxPatch     :: TVar Int -- max poll patch size
  , sKeepalive    :: TVar Int
  , sExpiration   :: TVar Int -- run job cache expiration
  , sAutoPoll     :: TVar Bool
  , sPolled       :: TVar Bool
  , sCleanup      :: IO ()
  , sFuncStatList :: FuncStatList
  , sLocker       :: L.Lock
  , sGrabQueue    :: GrabQueue
  , sAlive        :: TVar Bool
  , sChanList     :: TVar [Action]
  , sWaitList     :: WaitList
  , sLockList     :: LockList
  , sPersist      :: Persist
  }

newtype SchedT m a = SchedT {unSchedT :: ReaderT SchedEnv m a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , MonadReader SchedEnv
    )

instance MonadUnliftIO m => MonadUnliftIO (SchedT m) where
  askUnliftIO = SchedT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runSchedT r))
  withRunInIO inner = SchedT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runSchedT r)


type TaskList = IOHashMap JobHandle (Int64, Async ())

runSchedT :: SchedEnv -> SchedT m a -> m a
runSchedT schedEnv = flip runReaderT schedEnv . unSchedT

initSchedEnv :: MonadUnliftIO m => Persist -> m () -> m SchedEnv
initSchedEnv sPersist sC = do
  sFuncStatList <- newIOHashMap
  sWaitList     <- newIOHashMap
  sLockList     <- newIOHashMap
  sLocker       <- L.new
  sGrabQueue    <- newGrabQueue
  sAlive        <- newTVarIO True
  sChanList     <- newTVarIO []
  sPollDelay    <- newTVarIO 300
  sRevertDelay  <- newTVarIO 300
  sTaskTimeout  <- newTVarIO 600
  sMaxPatch     <- newTVarIO 250
  sKeepalive    <- newTVarIO 300
  sExpiration   <- newTVarIO 300
  sAutoPoll     <- newTVarIO False
  sPolled       <- newTVarIO False
  sCleanup      <- toIO sC
  pure SchedEnv{..}

startSchedT :: MonadUnliftIO m => SchedT m ()
startSchedT = do
  SchedEnv{..} <- ask
  runTask_ sRevertDelay revertRunningQueue
  taskList <- newIOHashMap
  runTask_ sPollDelay $ pushChanList PollJob
  runTask 0 $ runChanJob taskList
  runTask 100 purgeExpired

  loadInt "poll-delay" sPollDelay
  loadInt "revert-delay" sRevertDelay
  loadInt "timeout" sTaskTimeout
  loadInt "keepalive" sKeepalive
  loadInt "max-patch" sMaxPatch
  loadInt "expiration" sExpiration

loadInt :: MonadIO m => String -> TVar Int -> SchedT m ()
loadInt name ref = do
  v <- transactReadOnly $ flip P.configGet name
  case v of
    Nothing -> pure ()
    Just v' -> atomically $ writeTVar ref v'

saveInt :: MonadIO m => String -> Int -> TVar Int -> SchedT m ()
saveInt name v ref = do
  transact $ \p -> P.configSet p name v
  atomically $ writeTVar ref v

setConfigInt :: MonadIO m => String -> Int -> SchedT m ()
setConfigInt key val = do
  SchedEnv {..} <- ask
  case key of
    "poll-delay"   -> saveInt "poll-delay" val sPollDelay
    "revert-delay" -> saveInt "revert-delay" val sRevertDelay
    "timeout"      -> saveInt "timeout" val sTaskTimeout
    "keepalive"    -> saveInt "keepalive" val sKeepalive
    "max-patch"    -> saveInt "max-patch" val sMaxPatch
    "expiration"   -> saveInt "expiration" val sExpiration
    _              -> pure ()

getConfigInt :: MonadIO m => String -> SchedT m Int
getConfigInt key = do
  SchedEnv {..} <- ask
  case key of
    "poll-delay"   -> readTVarIO sPollDelay
    "revert-delay" -> readTVarIO sRevertDelay
    "timeout"      -> readTVarIO sTaskTimeout
    "keepalive"    -> readTVarIO sKeepalive
    "max-patch"    -> readTVarIO sMaxPatch
    "expiration"   -> readTVarIO sExpiration
    _              -> pure 0

keepalive :: Monad m => SchedT m (TVar Int)
keepalive = asks sKeepalive

runTask :: (MonadUnliftIO m) => Int -> SchedT m () -> SchedT m ()
runTask d m = flip runTask_ m =<< newTVarIO d

runTask_ :: (MonadUnliftIO m) => TVar Int -> SchedT m () -> SchedT m ()
runTask_ d m = void . async $ do
  SchedEnv{..} <- ask
  void . runMaybeT . forever $ do
    delay <- readTVarIO d
    when (delay > 0) $ threadDelay $ delay * 1000 * 1000
    alive <- readTVarIO sAlive
    if alive then lift m
             else mzero

withPersist :: MonadIO m => (Persist -> IO a) -> SchedT m a
withPersist f = do
  persist <- asks sPersist
  liftIO $ f persist

transact :: MonadIO m => (Persist -> IO a) -> SchedT m a
transact f = withPersist $ \persist ->
  P.transact persist $ f persist

transactReadOnly :: MonadIO m => (Persist -> IO a) -> SchedT m a
transactReadOnly f = withPersist $ \persist ->
  P.transactReadOnly persist $ f persist

runChanJob
  :: MonadUnliftIO m
  => TaskList -> SchedT m ()
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
          :: (MonadUnliftIO m)
          => TaskList -> Action -> SchedT m ()
        doChanJob tl (Add job)    = reSchedJob tl job
        doChanJob tl (Remove job) = findTask tl job >>= mapM_ cancel
        doChanJob tl Cancel       = mapM_ (cancel . snd) =<< FL.elems tl
        doChanJob tl PollJob      = pollJob tl
        doChanJob tl (TryPoll jh) = removeTaskAndTryPoll tl jh


pollDelay :: (MonadIO m, Num a) => SchedT m a
pollDelay = fmap fromIntegral . readTVarIO =<< asks sPollDelay

removeTaskAndTryPoll :: MonadIO m => TaskList -> JobHandle -> SchedT m ()
removeTaskAndTryPoll taskList jh = do
  FL.delete taskList jh
  polled <- asks sPolled
  isPolled <- readTVarIO polled
  autoPoll <- readTVarIO =<< asks sAutoPoll
  when (isPolled && autoPoll) $ do
    maxPatch <- readTVarIO =<< asks sMaxPatch
    size <- FL.size taskList
    when (size < maxPatch) $ do
      atomically $ writeTVar polled False
      pushChanList PollJob

pollJob
  :: (MonadUnliftIO m)
  => TaskList -> SchedT m ()
pollJob taskList = do
  polled <- asks sPolled
  atomically $ writeTVar polled False
  mapM_ checkPoll =<< FL.toList taskList
  stList <- asks sFuncStatList
  funcList <- foldr foldFunc [] <$> FL.toList stList
  pollJob_ taskList funcList
  atomically $ writeTVar polled True

  where foldFunc :: (FuncName, FuncStat) -> [FuncName] -> [FuncName]
        foldFunc (_, FuncStat{sWorker=0}) acc = acc
        foldFunc (fn, _) acc                  = fn:acc

        checkPoll
          :: (MonadIO m)
          => (JobHandle, (Int64, Async ())) -> SchedT m ()
        checkPoll (jh, (_, w)) = do
          r <- poll w
          case r of
            Just (Right ())  -> FL.delete taskList jh
            Just (Left e)  -> do
              FL.delete taskList jh
              liftIO $ errorM "Periodic.Server.Scheduler" ("Poll error: " ++ show e)
            Nothing -> do
              r0 <- canRun fn
              unless r0 $ cancel w

          where (fn, _) = unHandle jh

pollJob_
  :: MonadUnliftIO m
  => TaskList -> [FuncName] -> SchedT m ()
pollJob_ _ [] = pure ()
pollJob_ taskList funcList = do
  now <- getEpochTime
  next <- (+ (100 + now)) <$> pollDelay
  handles <- FL.keys taskList
  let check job = notElem (getHandle job) handles && (getSchedAt job < next)

  maxPatch <- readTVarIO =<< asks sMaxPatch
  jobs <- transactReadOnly $ \p ->
    P.foldr' p Pending funcList (foldFunc (maxPatch * 2) check now) PSQ.empty

  mapM_ (checkJob taskList) jobs

  autoPoll <- asks sAutoPoll
  atomically $ writeTVar autoPoll (length jobs > maxPatch)

  where foldFunc :: Int -> (Job -> Bool) -> Int64 -> Job -> HashPSQ JobHandle Int64 Job -> HashPSQ JobHandle Int64 Job
        foldFunc s f now job acc | f job = trimPSQ $ PSQ.insert (getHandle job) (now - getSchedAt job) job acc
                                 | otherwise = acc
          where trimPSQ :: HashPSQ JobHandle Int64 Job -> HashPSQ JobHandle Int64 Job
                trimPSQ q | PSQ.size q > s = trimPSQ $ PSQ.deleteMin q
                          | otherwise = q

        checkJob
          :: (MonadUnliftIO m)
          => TaskList -> Job -> SchedT m ()
        checkJob tl job = do
          w <- findTask tl job
          case w of
            Nothing -> do
              isProc <- transactReadOnly $ \p -> P.member p Running fn jn
              unless isProc $ reSchedJob tl job
            Just w0 -> do
              r <- canRun fn
              unless r $ cancel w0
          where fn = getFuncName job
                jn = getName job

pushChanList :: MonadIO m => Action -> SchedT m ()
pushChanList act = do
  cl <- asks sChanList
  atomically $ do
    l <- readTVar cl
    writeTVar cl (act:l)

pushJob :: MonadIO m => Job -> SchedT m ()
pushJob job = do
  isRunning <- transactReadOnly $ \p -> P.member p Running fn jn
  unless isRunning doPushJob

  where fn = getFuncName job
        jn = getName job

        doPushJob :: MonadIO m => SchedT m ()
        doPushJob = do
          pushChanList (Add job)
          transact $ \p -> P.insert p Pending fn jn job

reSchedJob :: MonadUnliftIO m => TaskList -> Job -> SchedT m ()
reSchedJob taskList job = do
  w <- findTask taskList job
  forM_ w cancel

  delay <- (+100) <$> pollDelay
  next <- (+ delay) <$> getEpochTime
  when (getSchedAt job < next) $ do
    r <- canRun $ getFuncName job
    c <- check taskList
    when (r && c) $ do
      w' <- schedJob taskList job
      FL.insert taskList (getHandle job) (getSchedAt job, w')
  where check :: (MonadIO m) => TaskList -> SchedT m Bool
        check tl = do
          maxPatch <- readTVarIO =<< asks sMaxPatch
          size <- FL.size tl
          if size < maxPatch * 2 then return True
          else do
            lastTask <- findLastTask tl
            case lastTask of
              Nothing -> return True
              Just (sc, jh, w) ->
                if sc < getSchedAt job then return False
                else do
                  cancel w
                  FL.delete taskList jh
                  return True

findTask :: MonadIO m => TaskList -> Job -> SchedT m (Maybe (Async ()))
findTask taskList job = fmap snd <$> FL.lookup taskList (getHandle job)

findLastTask :: MonadIO m => TaskList -> SchedT m (Maybe (Int64, JobHandle, Async ()))
findLastTask tl = atomically $ FL.foldrWithKeySTM tl f Nothing
  where f :: JobHandle
          -> (Int64, a)
          -> Maybe (Int64, JobHandle, a)
          -> Maybe (Int64, JobHandle, a)
        f jh (sc, t) Nothing = Just (sc, jh, t)
        f jh (sc, t) (Just (sc1, jh1, t1))
          | sc > sc1 = Just (sc, jh, t)
          | otherwise = Just (sc1, jh1, t1)

canRun :: MonadIO m => FuncName -> SchedT m Bool
canRun fn = asks sFuncStatList >>= flip canRun_ fn

canRun_ :: MonadIO m => FuncStatList -> FuncName -> m Bool
canRun_ stList fn = do
  st0 <- FL.lookup stList fn
  case st0 of
    Nothing                  -> pure False
    Just FuncStat{sWorker=0} -> pure False
    Just _                   -> pure True

schedJob
  :: MonadUnliftIO m
  => TaskList -> Job -> SchedT m (Async ())
schedJob taskList = async . schedJob_ taskList

schedJob_ :: MonadUnliftIO m => TaskList -> Job -> SchedT m ()
schedJob_ taskList job = do
  SchedEnv{..} <- ask
  r <- canRun fn
  when r $ do
    now <- getEpochTime
    when (schedAt > now) . threadDelay . fromIntegral $ (schedAt - now) * 1000000
    FuncStat{..} <- atomically $ do
      st <- FL.lookupSTM sFuncStatList fn
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
          :: MonadUnliftIO m => TaskList -> SchedT m ()
        popAgentThen tl = do
          SchedEnv{..} <- ask
          (jq, env0) <- atomically $ popAgentSTM sGrabQueue fn
          alive <- runAgentT' env0 aAlive
          if alive then do
            IL.insert jq (getHandle job)
            nextSchedAt <- getEpochTime
            transact $ \p -> P.insert p Running fn jn $ setSchedAt nextSchedAt job
            r <- doSubmitJob env0
            case r of
              Left _ -> do
                transact $ \p -> P.insert p Pending fn jn $ setSchedAt nextSchedAt job
                IL.delete jq jh
                schedJob_ tl job
              Right _ -> endSchedJob
          else schedJob_ tl job

        popAgentListThen :: MonadUnliftIO m => SchedT m ()
        popAgentListThen = do
          SchedEnv{..} <- ask
          agents <- popAgentList sGrabQueue fn
          mapM_ (doSubmitJob . snd) agents
          unless (null agents) endSchedJob -- wait to resched the broadcast job

        doSubmitJob :: MonadUnliftIO m => AgentEnv' -> SchedT m (Either SomeException ())
        doSubmitJob agent = do
          SchedEnv{..} <- ask
          tryAny $ assignJob agent job

        endSchedJob :: MonadIO m => SchedT m ()
        endSchedJob = pushChanList (TryPoll jh)

adjustFuncStat :: MonadIO m => FuncName -> SchedT m ()
adjustFuncStat fn = do
  (size, sizePQ, sizeL, sc) <- transactReadOnly $ \p -> do
    size <- P.size p Pending fn
    sizePQ <- P.size p Running fn
    sizeL <- P.size p Locking fn
    sc <- P.minSchedAt p fn
    pure (size, sizePQ, sizeL, sc)

  SchedEnv{..} <- ask
  schedAt <- if sc > 0 then pure sc else getEpochTime

  FL.alter sFuncStatList (update (size + sizePQ + sizeL) sizePQ sizeL schedAt) fn

  where update :: Int64 -> Int64 -> Int64 -> Int64 -> Maybe FuncStat -> Maybe FuncStat
        update size sizePQ sizeL schedAt st =
          Just ((fromMaybe (funcStat fn) st) { sJob = size
                                             , sRunning = sizePQ
                                             , sLocking = sizeL
                                             , sSchedAt = schedAt
                                             })

removeJob :: (MonadIO m) => Job -> SchedT m ()
removeJob job = do
  transact $ \p -> P.delete p fn jn

  pushChanList (Remove job)
  pushResult jh ""
  where jn = getName job
        fn = getFuncName job
        jh = getHandle job

dumpJob :: MonadIO m => SchedT m [Job]
dumpJob = transactReadOnly $ \p -> do
  js0 <- P.foldr p Pending (:) []
  js1 <- P.foldr p Running (:) []
  js2 <- P.foldr p Locking (:) []
  return $ js0 ++ js1 ++ js2

alterFunc :: MonadIO m => FuncName -> (Maybe FuncStat -> Maybe FuncStat) -> SchedT m ()
alterFunc n f = do
  SchedEnv{..} <- ask
  FL.alter sFuncStatList f n
  transact $ \p -> P.insertFuncName p n
  pushChanList PollJob

addFunc :: MonadIO m => FuncName -> SchedT m ()
addFunc n = broadcastFunc n False

broadcastFunc :: MonadIO m => FuncName -> Bool -> SchedT m ()
broadcastFunc n cast = alterFunc n updateStat

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just ((funcStat n) {sWorker = 1, sBroadcast = cast})
        updateStat (Just fs) = Just (fs { sWorker = sWorker fs + 1, sBroadcast = cast })

removeFunc :: MonadIO m => FuncName -> SchedT m ()
removeFunc n = alterFunc n updateStat

  where updateStat :: Maybe FuncStat -> Maybe FuncStat
        updateStat Nothing   = Just (funcStat n)
        updateStat (Just fs) = Just (fs { sWorker = max (sWorker fs - 1) 0 })

dropFunc :: MonadUnliftIO m => FuncName -> SchedT m ()
dropFunc n = do
  SchedEnv{..} <- ask
  L.with sLocker $ do
    st <- FL.lookup sFuncStatList n
    case st of
      Just FuncStat{sWorker=0} -> do
        FL.delete sFuncStatList n
        liftIO $ P.transact sPersist $ P.removeFuncName sPersist n
      _                        -> pure ()

  pushChanList PollJob

pushGrab :: MonadIO m => IOList FuncName -> IOList JobHandle -> AgentEnv' -> SchedT m ()
pushGrab funcList handleList ag = do
  queue <- asks sGrabQueue
  pushAgent queue funcList handleList ag

assignJob :: MonadUnliftIO m => AgentEnv' -> Job -> m ()
assignJob env0 job =
  runAgentT' env0 $ send (JobAssign job)

failJob :: (MonadIO m) => JobHandle -> SchedT m ()
failJob jh = do
  releaseLock' jh
  job <- transactReadOnly $ \p -> P.lookup p Running fn jn
  when (isJust job) $ do
    nextSchedAt <- getEpochTime
    retryJob $ setSchedAt nextSchedAt $ fromJust job

  where (fn, jn) = unHandle jh

retryJob :: MonadIO m => Job -> SchedT m ()
retryJob job = do
  transact $ \p -> P.insert p Pending fn jn job

  pushChanList (Add job)

  where  fn = getFuncName job
         jn = getName job

doneJob
  :: MonadIO m
  => JobHandle -> ByteString -> SchedT m ()
doneJob jh w = do
  releaseLock' jh
  transact $ \p -> P.delete p fn jn
  pushResult jh w
  where (fn, jn) = unHandle jh

schedLaterJob
  :: (MonadIO m)
  => JobHandle -> Int64 -> Int -> SchedT m ()
schedLaterJob jh later step = do
  releaseLock' jh
  job <- transactReadOnly $ \p -> P.lookup p Running fn jn
  when (isJust job) $ do
    let job' = fromJust job

    nextSchedAt <- (+) later <$> getEpochTime
    retryJob $ setCount (getCount job' + step) $ setSchedAt nextSchedAt job'

  where (fn, jn) = unHandle jh

acquireLock
  :: MonadIO m
  => LockName -> Int -> JobHandle -> SchedT m Bool
acquireLock name maxCount jh = do
  lockList <- asks sLockList
  j <- transactReadOnly $ \p -> P.lookup p Running fn jn
  case j of
    Nothing -> pure False
    Just job -> do
      r <- atomically $ do
        l <- FL.lookupSTM lockList name
        case l of
          Nothing -> do
            FL.insertSTM lockList name ([jh], [])
            pure True
          Just (acquired, locked) ->
            if length acquired < maxCount then do
              FL.insertSTM lockList name (L.nub $ acquired ++ [jh], locked)
              pure True
            else do
              FL.insertSTM lockList name (acquired, L.nub $ locked ++ [jh])
              pure False

      unless r $ transact $ \p -> P.insert p Locking fn jn job

      pure r

  where (fn, jn) = unHandle jh

releaseLock
  :: MonadIO m
  => LockName -> JobHandle -> SchedT m ()
releaseLock name jh = do
  lockList <- asks sLockList
  h <- atomically $ do
    l <- FL.lookupSTM lockList name
    case l of
      Nothing -> pure Nothing
      Just (acquired, locked) ->
        case L.uncons locked of
          Nothing -> do
            FL.insertSTM lockList name (L.delete jh acquired, [])
            pure Nothing
          Just (x, xs) -> do
            FL.insertSTM lockList name (L.delete jh acquired, xs)
            pure $ Just x

  case h of
    Nothing -> pure ()
    Just hh -> do
      let (fn, jn) = unHandle hh
      j <- transactReadOnly $ \p -> P.lookup p Locking fn jn
      case j of
        Nothing  -> releaseLock name hh
        Just job -> do
          transact $ \p -> P.insert p Pending fn jn job
          pushChanList (Add job)

releaseLock'
  :: MonadIO m
  => JobHandle -> SchedT m ()
releaseLock' jh = do
  lockList <- asks sLockList
  names <- atomically $ FL.foldrWithKeySTM lockList foldFunc []
  mapM_ (`releaseLock` jh) names

  where foldFunc :: LockName -> ([JobHandle], [JobHandle]) -> [LockName] -> [LockName]
        foldFunc n (acquired, _) acc | jh `elem` acquired = n : acc
                                     | otherwise = acc

status :: MonadIO m => SchedT m [FuncStat]
status = do
  mapM_ adjustFuncStat =<< transactReadOnly P.funcList
  FL.elems =<< asks sFuncStatList

revertRunningQueue :: (MonadIO m) => SchedT m ()
revertRunningQueue = do
  now <- getEpochTime
  tout <- fmap fromIntegral . readTVarIO =<< asks sTaskTimeout
  handles <- transactReadOnly $ \p -> P.foldr p Running (foldFunc (check now tout)) []
  mapM_ (failJob . getHandle) handles

  where foldFunc :: (Job -> Bool) -> Job -> [Job] -> [Job]
        foldFunc f job acc | f job = job : acc
                           | otherwise = acc

        check :: Int64 -> Int64 -> Job -> Bool
        check now t0 job
          | getTimeout job > 0 = getSchedAt job + fromIntegral (getTimeout job) < now
          | otherwise = getSchedAt job + fromIntegral t0 < now

purgeExpired :: MonadIO m => SchedT m ()
purgeExpired = do
  now <- getEpochTime
  wl <- asks sWaitList
  ex <- fmap fromIntegral . readTVarIO =<< asks sExpiration
  atomically $ do
    ks <- FL.foldrWithKeySTM wl (foldFunc (check (now - ex))) []
    mapM_ (FL.deleteSTM wl) ks

  where foldFunc :: ((Int64, Maybe ByteString) -> Bool) -> JobHandle -> (Int64, Maybe ByteString) -> [JobHandle] -> [JobHandle]
        foldFunc f jh v acc | f v = jh : acc
                           | otherwise = acc

        check :: Int64 -> (Int64, Maybe ByteString) -> Bool
        check t0 (t, _) = t < t0

shutdown :: (MonadUnliftIO m) => SchedT m ()
shutdown = do
  SchedEnv{..} <- ask
  pushChanList Cancel
  alive <- atomically $ do
    t <- readTVar sAlive
    writeTVar sAlive False
    return t
  when alive . void . async $ liftIO sCleanup

prepareWait :: MonadIO m => Job -> SchedT m ()
prepareWait job = pushResult_ updateWL jh
  where updateWL :: Int64 -> Maybe (Int64, Maybe ByteString) -> Maybe (Int64, Maybe ByteString)
        updateWL now Nothing       = Just (now, Nothing)
        updateWL now (Just (_, v)) = Just (now, v)

        jh = getHandle job

waitResult :: MonadIO m => TVar Bool -> Job -> SchedT m ByteString
waitResult state job = do
  wl <- asks sWaitList
  atomically $ do
    st <- readTVar state
    if st then do
      w0 <- FL.lookupSTM wl jh
      case w0 of
        Just (_, Just w1) -> pure w1
        Just (_, Nothing) -> retrySTM
        Nothing           -> pure ""
     else pure ""

  where jh = getHandle job

pushResult
  :: MonadIO m
  => JobHandle -> ByteString -> SchedT m ()
pushResult jh w = pushResult_ updateWL jh
  where updateWL :: Int64 -> Maybe (Int64, Maybe ByteString) -> Maybe (Int64, Maybe ByteString)
        updateWL _ Nothing    = Nothing
        updateWL now (Just _) = Just (now, Just w)

pushResult_
  :: MonadIO m
  => (Int64 -> Maybe (Int64, Maybe ByteString) -> Maybe (Int64, Maybe ByteString))
  -> JobHandle -> SchedT m ()
pushResult_ f jh = do
  wl <- asks sWaitList
  now <- getEpochTime
  FL.alter wl (f now) jh
