{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
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

import           Control.Exception               (SomeException, try)
import           Control.Monad                   (forever, mzero, unless, void,
                                                  when)
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
import           Periodic.Types.Internal         (LockName)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand    (ServerCommand (JobAssign))
import           Periodic.Utils                  (getEpochTime)

import           System.Directory                (doesFileExist)
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

import           Periodic.Server.Persist         (Persist, State (..))
import qualified Periodic.Server.Persist         as P

import           Data.ByteString                 (ByteString)
import           Data.Foldable                   (forM_)

import           Data.HashPSQ                    (HashPSQ)
import qualified Data.HashPSQ                    as PSQ
import qualified Data.List                       as L (delete, nub, uncons)
import           System.Log.Logger               (errorM)

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
  , sStorePath    :: FilePath
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
type TaskList m = IOHashMap JobHandle (Int64, Task m)

runSchedT :: SchedEnv -> SchedT m a -> m a
runSchedT schedEnv = flip runReaderT schedEnv . unSchedT

initSchedEnv :: FilePath -> Persist -> IO () -> IO SchedEnv
initSchedEnv sStorePath sPersist sCleanup = do
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
  pure SchedEnv{..}

startSchedT
  :: (MonadIO m, MonadBaseControl IO m)
  => SchedT m ()
startSchedT = do
  SchedEnv{..} <- ask
  runTask_ sRevertDelay revertProcessQueue
  taskList <- liftIO newIOHashMap
  runTask_ sPollDelay $ pushChanList PollJob
  runTask 0 $ runChanJob taskList
  runTask 100 purgeExpired

  loadInt "poll-delay" sPollDelay
  loadInt "revert-delay" sRevertDelay
  loadInt "timeout" sTaskTimeout
  loadInt "keepalive" sKeepalive
  loadInt "max-patch" sMaxPatch
  loadInt "expiration" sExpiration

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
    "keepalive"    -> saveInt "keepalive" val sKeepalive
    "max-patch"    -> saveInt "max-patch" val sMaxPatch
    "expiration"   -> saveInt "expiration" val sExpiration
    _              -> pure ()

getConfigInt :: MonadIO m => String -> SchedT m Int
getConfigInt key = do
  SchedEnv {..} <- ask
  case key of
    "poll-delay"   -> liftIO $ readTVarIO sPollDelay
    "revert-delay" -> liftIO $ readTVarIO sRevertDelay
    "timeout"      -> liftIO $ readTVarIO sTaskTimeout
    "keepalive"    -> liftIO $ readTVarIO sKeepalive
    "max-patch"    -> liftIO $ readTVarIO sMaxPatch
    "expiration"   -> liftIO $ readTVarIO sExpiration
    _              -> pure 0

keepalive :: Monad m => SchedT m (TVar Int)
keepalive = asks sKeepalive

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
  :: (MonadIO m, MonadBaseControl IO m)
  => TaskList m -> SchedT m ()
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

  where doChanJob
          :: (MonadIO m, MonadBaseControl IO m)
          => TaskList m -> Action -> SchedT m ()
        doChanJob tl (Add job)    = reSchedJob tl job
        doChanJob tl (Remove job) = findTask tl job >>= mapM_ cancel
        doChanJob tl Cancel       = mapM_ (cancel . snd) =<< liftIO (FL.elems tl)
        doChanJob tl PollJob      = pollJob tl
        doChanJob tl (TryPoll jh) = removeTaskAndTryPoll tl jh


pollDelay :: (MonadIO m, Num a) => SchedT m a
pollDelay = liftIO . fmap fromIntegral . readTVarIO =<< asks sPollDelay

removeTaskAndTryPoll :: MonadIO m => TaskList m -> JobHandle -> SchedT m ()
removeTaskAndTryPoll taskList jh = do
  liftIO $ FL.delete taskList jh
  polled <- asks sPolled
  isPolled <- liftIO $ readTVarIO polled
  autoPoll <- liftIO . readTVarIO =<< asks sAutoPoll
  when (isPolled && autoPoll) $ do
    maxPatch <- liftIO . readTVarIO =<< asks sMaxPatch
    size <- liftIO $ FL.size taskList
    when (size < maxPatch) $ do
      liftIO . atomically $ writeTVar polled False
      pushChanList PollJob

pollJob
  :: (MonadIO m, MonadBaseControl IO m)
  => TaskList m -> SchedT m ()
pollJob taskList = do
  polled <- asks sPolled
  liftIO . atomically $ writeTVar polled False
  mapM_ checkPoll =<< liftIO (FL.toList taskList)
  stList <- asks sFuncStatList
  funcList <- liftIO $ foldr foldFunc [] <$> FL.toList stList
  pollJob_ taskList funcList
  liftIO . atomically $ writeTVar polled True

  where foldFunc :: (FuncName, FuncStat) -> [FuncName] -> [FuncName]
        foldFunc (_, FuncStat{sWorker=0}) acc = acc
        foldFunc (fn, _) acc                  = fn:acc

        checkPoll
          :: (MonadIO m, MonadBaseControl IO m)
          => (JobHandle, (Int64, Task m)) -> SchedT m ()
        checkPoll (jh, (_, w)) = do
          r <- poll w
          case r of
            Just (Right ())  -> liftIO $ FL.delete taskList jh
            Just (Left e)  ->
              liftIO $ FL.delete taskList jh
                     >> errorM "Periodic.Server.Scheduler" ("Poll error: " ++ show e)
            Nothing -> do
              r0 <- canRun fn
              unless r0 $ cancel w

          where (fn, _) = unHandle jh

pollJob_
  :: (MonadIO m, MonadBaseControl IO m)
  => TaskList m -> [FuncName] -> SchedT m ()
pollJob_ _ [] = pure ()
pollJob_ taskList funcList = do
  now <- liftIO getEpochTime
  next <- (+ (100 + now)) <$> pollDelay
  handles <- liftIO $ FL.keys taskList
  let check job = notElem (getHandle job) handles && (getSchedAt job < next)

  maxPatch <- liftIO . readTVarIO =<< asks sMaxPatch
  jobs <- transactReadOnly $ \p ->
    P.foldr' p Pending funcList (foldFunc (maxPatch * 2) check now) PSQ.empty

  mapM_ (checkJob taskList) jobs

  autoPoll <- asks sAutoPoll
  liftIO . atomically $ writeTVar autoPoll (length jobs > maxPatch)

  where foldFunc :: Int -> (Job -> Bool) -> Int64 -> Job -> HashPSQ JobHandle Int64 Job -> HashPSQ JobHandle Int64 Job
        foldFunc s f now job acc | f job = trimPSQ $ PSQ.insert (getHandle job) (now - getSchedAt job) job acc
                                 | otherwise = acc
          where trimPSQ :: HashPSQ JobHandle Int64 Job -> HashPSQ JobHandle Int64 Job
                trimPSQ q | PSQ.size q > s = trimPSQ $ PSQ.deleteMin q
                          | otherwise = q

        checkJob
          :: (MonadIO m, MonadBaseControl IO m)
          => TaskList m -> Job -> SchedT m ()
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
  liftIO . atomically $ do
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

reSchedJob :: (MonadIO m, MonadBaseControl IO m) => TaskList m -> Job -> SchedT m ()
reSchedJob taskList job = do
  w <- findTask taskList job
  forM_ w cancel

  delay <- (+100) <$> pollDelay
  next <- liftIO $ (+ delay) <$> getEpochTime
  when (getSchedAt job < next) $ do
    r <- canRun $ getFuncName job
    c <- check taskList
    when (r && c) $ do
      w' <- schedJob taskList job
      liftIO $ FL.insert taskList (getHandle job) (getSchedAt job, w')
  where check :: (MonadIO m, MonadBaseControl IO m) => TaskList m -> SchedT m Bool
        check tl = do
          maxPatch <- liftIO . readTVarIO =<< asks sMaxPatch
          size <- liftIO $ FL.size tl
          if size < maxPatch * 2 then return True
          else do
            lastTask <- findLastTask tl
            case lastTask of
              Nothing -> return True
              Just (sc, jh, w) ->
                if sc < getSchedAt job then return False
                else do
                  cancel w
                  liftIO $ FL.delete taskList jh
                  return True

findTask :: MonadIO m => TaskList m -> Job -> SchedT m (Maybe (Task m))
findTask taskList job = liftIO $ fmap snd <$> FL.lookup taskList (getHandle job)

findLastTask :: MonadIO m => TaskList m -> SchedT m (Maybe (Int64, JobHandle, Task m))
findLastTask tl = liftIO . atomically $ FL.foldrWithKeySTM tl f Nothing
  where f :: JobHandle
          -> (Int64, a)
          -> Maybe (Int64, JobHandle, a)
          -> Maybe (Int64, JobHandle, a)
        f jh (sc, t) Nothing = Just (sc, jh, t)
        f jh (sc, t) (Just (sc1, jh1, t1))
          | sc > sc1 = Just (sc, jh, t)
          | otherwise = Just (sc1, jh1, t1)

canRun :: MonadIO m => FuncName -> SchedT m Bool
canRun fn = asks sFuncStatList >>= liftIO . flip canRun_ fn

canRun_ :: FuncStatList -> FuncName -> IO Bool
canRun_ stList fn = do
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
schedJob_ taskList job = do
  SchedEnv{..} <- ask
  r <- canRun fn
  when r $ do
    now <- liftIO getEpochTime
    when (schedAt > now) . liftIO . threadDelay . fromIntegral $ (schedAt - now) * 1000000
    FuncStat{..} <- liftIO . atomically $ do
      st <- FL.lookupSTM sFuncStatList fn
      case st of
        Nothing                  -> retry
        Just FuncStat{sWorker=0} -> retry
        Just st'                 -> pure st'
    if sBroadcast then popAgentListThen
                  else popAgentThen taskList

  where fn = getFuncName job
        jn = getName job
        schedAt = getSchedAt job
        jh = getHandle job

        popAgentThen
          :: MonadIO m => TaskList m -> SchedT m ()
        popAgentThen tl = do
          SchedEnv{..} <- ask
          (jq, env0) <- liftIO $ atomically $ popAgentSTM sGrabQueue fn
          alive <- liftIO $ runAgentT' env0 aAlive
          if alive then do
            liftIO $ IL.insert jq (getHandle job)
            nextSchedAt <- liftIO getEpochTime
            transact $ \p -> P.insert p Running fn jn $ setSchedAt nextSchedAt job
            r <- doSubmitJob env0
            case r of
              Left _ -> do
                transact $ \p -> P.insert p Pending fn jn $ setSchedAt nextSchedAt job
                liftIO $ IL.delete jq jh
                schedJob_ tl job
              Right _ -> endSchedJob
          else schedJob_ tl job

        popAgentListThen :: MonadIO m => SchedT m ()
        popAgentListThen = do
          SchedEnv{..} <- ask
          agents <- liftIO $ popAgentList sGrabQueue fn
          mapM_ (doSubmitJob . snd) agents
          unless (null agents) endSchedJob -- wait to resched the broadcast job

        doSubmitJob :: MonadIO m => AgentEnv' -> SchedT m (Either SomeException ())
        doSubmitJob agent = do
          SchedEnv{..} <- ask
          liftIO . try $ assignJob agent job

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
  schedAt <- if sc > 0 then pure sc else liftIO getEpochTime

  liftIO $ FL.alter sFuncStatList (update (size + sizePQ + sizeL) sizePQ sizeL schedAt) fn

  where update :: Int64 -> Int64 -> Int64 -> Int64 -> Maybe FuncStat -> Maybe FuncStat
        update size sizePQ sizeL schedAt st =
          Just ((fromMaybe (funcStat fn) st) { sJob = size
                                             , sProcess = sizePQ
                                             , sLocking = sizeL
                                             , sSchedAt = schedAt
                                             })

removeJob :: (MonadIO m, MonadBaseControl IO m) => Job -> SchedT m ()
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
  liftIO $ FL.alter sFuncStatList f n
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

dropFunc :: MonadIO m => FuncName -> SchedT m ()
dropFunc n = do
  SchedEnv{..} <- ask
  liftIO . L.with sLocker $ do
    st <- FL.lookup sFuncStatList n
    case st of
      Just FuncStat{sWorker=0} -> do
        FL.delete sFuncStatList n
        P.transact sPersist $ P.removeFuncName sPersist n
      _                        -> pure ()

  pushChanList PollJob

pushGrab :: MonadIO m => IOList FuncName -> IOList JobHandle -> AgentEnv' -> SchedT m ()
pushGrab funcList handleList ag = do
  queue <- asks sGrabQueue
  liftIO $ pushAgent queue funcList handleList ag

assignJob :: AgentEnv' -> Job -> IO ()
assignJob env0 job =
  runAgentT' env0 $ send (JobAssign job)

failJob :: (MonadIO m, MonadBaseControl IO m) => JobHandle -> SchedT m ()
failJob jh = do
  releaseLock' jh
  job <- transactReadOnly $ \p -> P.lookup p Running fn jn
  when (isJust job) $ do
    nextSchedAt <- liftIO getEpochTime
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
  :: (MonadIO m, MonadBaseControl IO m)
  => JobHandle -> Int64 -> Int -> SchedT m ()
schedLaterJob jh later step = do
  releaseLock' jh
  job <- transactReadOnly $ \p -> P.lookup p Running fn jn
  when (isJust job) $ do
    let job' = fromJust job

    nextSchedAt <- liftIO $ (+) later <$> getEpochTime
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
      r <- liftIO $ atomically $ do
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

      when (not r) $ transact $ \p -> P.insert p Locking fn jn job

      pure r

  where (fn, jn) = unHandle jh

releaseLock
  :: MonadIO m
  => LockName -> JobHandle -> SchedT m ()
releaseLock name jh = do
  lockList <- asks sLockList
  h <- liftIO $ atomically $ do
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
  names <- liftIO $ atomically $ FL.foldrWithKeySTM lockList foldFunc []
  mapM_ (flip releaseLock jh) names

  where foldFunc :: LockName -> ([JobHandle], [JobHandle]) -> [LockName] -> [LockName]
        foldFunc n (acquired, _) acc | elem jh acquired = n : acc
                                     | otherwise = acc

status :: MonadIO m => SchedT m [FuncStat]
status = do
  mapM_ adjustFuncStat =<< transactReadOnly P.funcList
  liftIO . FL.elems =<< asks sFuncStatList

revertProcessQueue :: (MonadIO m, MonadBaseControl IO m) => SchedT m ()
revertProcessQueue = do
  now <- liftIO getEpochTime
  tout <- liftIO . fmap fromIntegral . readTVarIO =<< asks sTaskTimeout
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
  now <- liftIO getEpochTime
  wl <- asks sWaitList
  ex <- liftIO . fmap fromIntegral . readTVarIO =<< asks sExpiration
  liftIO $ atomically $ do
    ks <- FL.foldrWithKeySTM wl (foldFunc (check (now - ex))) []
    mapM_ (FL.deleteSTM wl) ks

  where foldFunc :: ((Int64, Maybe ByteString) -> Bool) -> JobHandle -> (Int64, Maybe ByteString) -> [JobHandle] -> [JobHandle]
        foldFunc f jh v acc | f v = jh : acc
                           | otherwise = acc

        check :: Int64 -> (Int64, Maybe ByteString) -> Bool
        check t0 (t, _) = t < t0

shutdown :: (MonadIO m, MonadBaseControl IO m) => SchedT m ()
shutdown = do
  SchedEnv{..} <- ask
  pushChanList Cancel
  alive <- liftIO $ atomically $ do
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
  liftIO $ atomically $ do
    st <- readTVar state
    if st then do
      w0 <- FL.lookupSTM wl jh
      case w0 of
        Just (_, Just w1) -> pure w1
        Just (_, Nothing) -> retry
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
  now <- liftIO getEpochTime
  liftIO $ FL.alter wl (f now) jh
