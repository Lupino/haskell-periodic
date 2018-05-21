{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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
  ( Schema
  , emptySchema
  , SchedT
  , runSchedT
  , SchedEnv
  , initSchedEnv
  , startSchedT
  , pushJob
  , pushGrab
  , failJob
  , doneJob
  , schedLaterJob
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
  ) where

import           Control.Exception                (SomeException, try)
import           Control.Monad                    (forever, mzero, unless, void,
                                                   when)
import           Data.Int                         (Int64)
import           Data.Maybe                       (fromJust, fromMaybe, isJust)
import           Periodic.Agent                   (AgentEnv', aAlive,
                                                   runAgentT', send)
import           Periodic.IOHashMap               (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap               as FL
import           Periodic.IOList                  (IOList)
import qualified Periodic.IOList                  as IL
import qualified Periodic.Lock                    as L (Lock, new, with, withM)
import           Periodic.Server.FuncStat
import           Periodic.Server.GrabQueue
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand     (ServerCommand (JobAssign))
import           Periodic.Utils                   (getEpochTime)

import           System.Directory                 (createDirectoryIfMissing,
                                                   doesFileExist)
import           System.FilePath                  ((</>))

import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.Async.Lifted  (Async, async, cancel, poll)
import           Control.Concurrent.STM.TVar
import           Control.Monad.STM                (atomically, retry)

import           Control.Monad.Base
import           Control.Monad.Catch              (MonadCatch, MonadMask,
                                                   MonadThrow)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Reader.Class       (MonadReader (ask), asks)
import           Control.Monad.Trans.Class        (MonadTrans, lift)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Maybe        (runMaybeT)
import           Control.Monad.Trans.Reader       (ReaderT, runReaderT)

import           System.IO                        (readFile, writeFile)
import           Text.Read                        (readMaybe)

import           Control.Lens                     (Lens', lens, (%%~), (^.))
import           Control.Monad.Haskey
import           Data.Binary                      (Binary)
import           Data.BTree.Alloc                 (AllocM, AllocReaderM)
import           Data.BTree.Impure                (Tree)
import qualified Data.BTree.Impure                as B
import           Data.BTree.Primitives            (Key, Value)
import           Data.ByteString                  (ByteString)
import           Data.Typeable                    (Typeable)
import           Database.Haskey.Alloc.Concurrent (Root)
import           GHC.Generics                     (Generic)

data Action = Add Job | Remove Job | Cancel | PollJob

data SchedEnv = SchedEnv
  { sPollDelay    :: TVar Int -- main poll loop every time delay
  , sRevertDelay  :: TVar Int -- revert process queue loop every time delay
  , sTaskTimeout  :: TVar Int -- the task do timeout
  , sKeepalive    :: TVar Int
  , sStorePath    :: FilePath
  , sCleanup      :: IO ()
  , sFuncStatList :: FuncStatList
  , sLocker       :: L.Lock
  , sGrabQueue    :: GrabQueue
  , sAlive        :: TVar Bool
  , sChanList     :: TVar [Action]
  }

type JobTree = Tree JobName Job
type ProcTree = Tree ByteString Job

data Schema = Schema
  { _schemaJobTrees  :: Tree FuncName JobTree
  , _schemaProcTrees :: Tree FuncName ProcTree
  } deriving (Generic, Show, Typeable)

instance Binary Schema
instance Value Schema
instance Root Schema

emptySchema :: Schema
emptySchema = Schema B.empty B.empty

schemaJobTrees :: Lens' Schema (Tree FuncName JobTree)
schemaJobTrees = lens _schemaJobTrees $ \s x -> s { _schemaJobTrees = x }

schemaProcTrees :: Lens' Schema (Tree FuncName ProcTree)
schemaProcTrees = lens _schemaProcTrees $ \s x -> s { _schemaProcTrees = x }

updateJobTree
  :: (AllocM n, AllocReaderM n) => (Tree FuncName JobTree -> n (Tree FuncName JobTree)) -> Schema -> n Schema
updateJobTree f = schemaJobTrees %%~ f

queryJobTree
  :: AllocReaderM n => (Tree FuncName JobTree -> n a) -> Schema -> n a
queryJobTree f root = f (root ^. schemaJobTrees)

updateProcTree
  :: (AllocM n, AllocReaderM n) => (Tree FuncName ProcTree -> n (Tree FuncName ProcTree)) -> Schema -> n Schema
updateProcTree f = schemaProcTrees %%~ f

queryProcTree
  :: AllocReaderM n => (Tree FuncName ProcTree -> n a) -> Schema -> n a
queryProcTree f root = f (root ^. schemaProcTrees)

insertTree
  :: (AllocM n, AllocReaderM n, Key k)
  => FuncName -> k -> Job -> Tree FuncName (Tree k Job) -> n (Tree FuncName (Tree k Job))
insertTree fn k job tree =
  fromMaybe B.empty <$> B.lookup fn tree
    >>= B.insert k job
    >>= flip (B.insert fn) tree

deleteTree
  :: (AllocM n, AllocReaderM n, Key k)
  => FuncName -> k -> Tree FuncName (Tree k Job) -> n (Tree FuncName (Tree k Job))
deleteTree fn jn tree =
  fromMaybe B.empty <$> B.lookup fn tree
    >>= B.delete jn
    >>= flip (B.insert fn) tree

sizeTree :: (AllocReaderM n, Key k) => FuncName -> Tree FuncName (Tree k Job) -> n Int64
sizeTree fn tree =
  fromMaybe B.empty <$> B.lookup fn tree
    >>= B.foldr (\_ acc -> acc + 1) 0

minSchedAt :: (AllocReaderM n, Key k) => FuncName -> Tree FuncName (Tree k Job) -> n Int64
minSchedAt fn tree =
  fromMaybe B.empty <$> B.lookup fn tree
    >>= B.foldr (\j acc -> if acc == 0  || acc > jSchedAt j then jSchedAt j else acc) 0

foldrTree :: (AllocReaderM n, Key k) => (Job -> a -> a) -> a -> Tree FuncName (Tree k Job) -> n [a]
foldrTree f a0 = B.foldrM (\t0 acc -> (: acc) <$> B.foldr f a0 t0) []

lookupJob :: (AllocReaderM n, Key k) => FuncName -> k -> Tree FuncName (Tree k Job) -> n (Maybe Job)
lookupJob fn jn tree = fromMaybe B.empty <$> B.lookup fn tree >>= B.lookup jn

memberJob :: (AllocReaderM n, Key k) => FuncName -> k -> Tree FuncName (Tree k Job) -> n Bool
memberJob fn jn = fmap isJust . lookupJob fn jn

treeFuncList :: (AllocReaderM n, Key k) => Tree FuncName (Tree k Job) -> n [FuncName]
treeFuncList = B.foldrWithKey (\k _ acc -> k:acc) []

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

instance MonadHaskey root m => MonadHaskey root (SchedT m) where
  transact tx = lift $ transact tx
  transact_ tx = lift $ transact_ tx
  transactReadOnly tx = lift $ transactReadOnly tx

type Task m = Async (StM (SchedT m) ())
type TaskList m = IOHashMap JobHandle (Task m)

runSchedT :: SchedEnv -> SchedT m a -> m a
runSchedT schedEnv = flip runReaderT schedEnv . unSchedT

initSchedEnv :: FilePath -> IO () -> IO SchedEnv
initSchedEnv sStorePath sCleanup = do
  createDirectoryIfMissing True sStorePath
  sFuncStatList <- newIOHashMap
  sLocker       <- L.new
  sGrabQueue    <- newGrabQueue
  sAlive        <- newTVarIO True
  sChanList     <- newTVarIO []
  sPollDelay    <- newTVarIO 300
  sRevertDelay  <- newTVarIO 300
  sTaskTimeout  <- newTVarIO 600
  sKeepalive    <- newTVarIO 300
  pure SchedEnv{..}

startSchedT
  :: (MonadIO m, MonadBaseControl IO m, MonadMask m, MonadHaskey Schema m)
  => SchedT m ()
startSchedT = do
  SchedEnv{..} <- ask
  runTask_ sRevertDelay revertProcessQueue
  taskList <- liftIO newIOHashMap
  runTask_ sPollDelay $ pollJob taskList
  runTask 0 $ runChanJob taskList

  loadInt "poll-delay" sPollDelay
  loadInt "revert-delay" sRevertDelay
  loadInt "timeout" sTaskTimeout
  loadInt "keepalive" sKeepalive

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
    _              -> pure ()

getConfigInt :: MonadIO m => String -> SchedT m Int
getConfigInt key = do
  SchedEnv {..} <- ask
  case key of
    "poll-delay"   -> liftIO $ readTVarIO sPollDelay
    "revert-delay" -> liftIO $ readTVarIO sRevertDelay
    "timeout"      -> liftIO $ readTVarIO sTaskTimeout
    "keepalive"    -> liftIO $ readTVarIO sKeepalive
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

runChanJob
  :: (MonadIO m, MonadBaseControl IO m, MonadMask m, MonadHaskey Schema m)
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
          :: (MonadIO m, MonadBaseControl IO m, MonadMask m, MonadHaskey Schema m)
          => TaskList m -> Action -> SchedT m ()
        doChanJob tl (Add job) = reSchedJob tl job
        doChanJob tl (Remove job) = do
          w <- findTask tl job
          when (isJust w) $ do
            cancel (fromJust w)
            liftIO $ FL.delete tl (jHandle job)
        doChanJob tl Cancel = mapM_ cancel =<< liftIO (FL.elems tl)
        doChanJob tl PollJob = pollJob tl


pollDelay :: (MonadIO m, Num a) => SchedT m a
pollDelay = do
  d <- asks sPollDelay
  liftIO $ fromIntegral <$> readTVarIO d

pollJob
  :: (MonadIO m, MonadBaseControl IO m, MonadMask m, MonadHaskey Schema m)
  => TaskList m -> SchedT m ()
pollJob taskList = do
  SchedEnv{..} <- ask
  mapM_ checkPoll =<< liftIO (FL.toList taskList)

  delay <- (+100) <$> pollDelay

  next <- liftIO $ (+ delay) <$> getEpochTime
  jobs <- transactReadOnly $ queryJobTree $ foldrTree (foldFunc next) []
  L.withM sLocker $ mapM_ (checkJob taskList) $ concat jobs

  where foldFunc :: Int64 -> Job -> [Job] -> [Job]
        foldFunc t job acc | jSchedAt job < t = job : acc
                           | otherwise = acc

        checkJob
          :: (MonadIO m, MonadBaseControl IO m, MonadHaskey Schema m)
          => TaskList m -> Job -> SchedT m ()
        checkJob tl job@Job{..} = do
          w <- findTask tl job
          case w of
            Nothing -> do
              isProc <- transactReadOnly $ queryProcTree (memberJob jFuncName (hashJobName jName))
              unless isProc $ reSchedJob tl job
            Just w0 -> do
              r <- canRun jFuncName
              unless r $ cancel w0

        checkPoll
          :: (MonadIO m, MonadBaseControl IO m)
          => (JobHandle, Async (StM (SchedT m) ())) -> SchedT m ()
        checkPoll (jh, w) = do
          r <- poll w
          case r of
            Just (Right ()) -> liftIO $ FL.delete taskList jh
            _               -> pure ()

pushChanList :: MonadIO m => Action -> SchedT m ()
pushChanList act = do
  cl <- asks sChanList
  liftIO . atomically $ do
    l <- readTVar cl
    writeTVar cl (act:l)

pushJob :: (MonadIO m, MonadHaskey Schema m) => Job -> SchedT m ()
pushJob job@Job{..} = do
  exists <- transactReadOnly $ queryJobTree $ memberJob jFuncName jName
  if exists then doPushJob
            else do
              isProc <- transactReadOnly $ queryProcTree (memberJob jFuncName (hashJobName jName))
              unless isProc doPushJob

  where doPushJob :: (MonadIO m, MonadHaskey Schema m) => SchedT m ()
        doPushJob = do
          pushChanList (Add job)
          transact_ $ \schema ->
            updateJobTree (insertTree jFuncName jName job) schema
              >>= commit_

reSchedJob :: (MonadIO m, MonadBaseControl IO m, MonadHaskey Schema m) => TaskList m -> Job -> SchedT m ()
reSchedJob taskList job = do
  w <- findTask taskList job
  when (isJust w) $ do
    cancel (fromJust w)
    liftIO $ FL.delete taskList (jHandle job)

  delay <- (+100) <$> pollDelay
  next <- liftIO $ (+ delay) <$> getEpochTime
  when (jSchedAt job < next) $ do
    r <- canRun $ jFuncName job
    when r $ do
      w' <- schedJob taskList job
      liftIO $ FL.insert taskList (jHandle job) w'

findTask :: (MonadIO m) => TaskList m -> Job -> SchedT m (Maybe (Task m))
findTask taskList job = liftIO $ FL.lookup taskList (jHandle job)

canRun :: MonadIO m => FuncName -> SchedT m Bool
canRun fn = do
  stList <- asks sFuncStatList
  st0 <- liftIO $ FL.lookup stList fn
  case st0 of
    Nothing                  -> pure False
    Just FuncStat{sWorker=0} -> pure False
    Just _                   -> pure True

schedJob
  :: (MonadIO m, MonadBaseControl IO m, MonadHaskey Schema m)
  => TaskList m -> Job -> SchedT m (Task m)
schedJob taskList = async . schedJob_ taskList

schedJob_ :: (MonadIO m, MonadHaskey Schema m) => TaskList m -> Job -> SchedT m ()
schedJob_ taskList job@Job{..} = do
  SchedEnv{..} <- ask
  r <- canRun jFuncName
  when r $ do
    now <- liftIO getEpochTime
    when (jSchedAt > now) . liftIO . threadDelay . fromIntegral $ (jSchedAt - now) * 1000000
    FuncStat{..} <- liftIO . atomically $ do
      st <- FL.lookupSTM sFuncStatList jFuncName
      case st of
        Nothing                  -> retry
        Just FuncStat{sWorker=0} -> retry
        Just st'                 -> pure st'
    if sBroadcast then popAgentListThen taskList
                  else popAgentThen taskList

  where popAgentThen
          :: (MonadIO m, MonadHaskey Schema m) => TaskList m -> SchedT m ()
        popAgentThen tl = do
          SchedEnv{..} <- ask
          (jq, env0) <- liftIO $ atomically $ popAgentSTM sGrabQueue jFuncName
          alive <- liftIO $ runAgentT' env0 aAlive
          if alive then do
            liftIO $ IL.insert jq (jHandle job)
            nextSchedAt <- liftIO getEpochTime
            transact_ $ \schema ->
              updateProcTree (insertTree jFuncName (hashJobName jName) job {jSchedAt = nextSchedAt}) schema
                >>= commit_
            r <- doSubmitJob env0
            case r of
              Left _ -> do
                transact_ $ \schema ->
                  updateProcTree (deleteTree jFuncName (hashJobName jName)) schema
                    >>= commit_
                liftIO $ IL.delete jq (jHandle job)
                schedJob_ tl job
              Right _ -> do
                endSchedJob tl
          else schedJob_ tl job

        popAgentListThen :: (MonadIO m, MonadHaskey Schema m) => TaskList m -> SchedT m ()
        popAgentListThen tl = do
          SchedEnv{..} <- ask
          agents <- liftIO $ popAgentList sGrabQueue jFuncName
          mapM_ (doSubmitJob . snd) agents
          unless (null agents) $ endSchedJob tl -- wait to resched the broadcast job

        doSubmitJob :: MonadIO m => AgentEnv' -> SchedT m (Either SomeException ())
        doSubmitJob agent = do
          SchedEnv{..} <- ask
          liftIO . try $ assignJob agent job

        endSchedJob :: (MonadIO m, MonadHaskey Schema m) => TaskList m -> SchedT m ()
        endSchedJob tl = do
          transact_ $ \schema ->
            updateJobTree (deleteTree jFuncName jName) schema
              >>= commit_

          -- block on pollJob
          lock <- asks sLocker
          liftIO $ L.with lock $ FL.delete tl (jHandle job)

adjustFuncStat :: (MonadIO m, MonadHaskey Schema m) => FuncName -> SchedT m ()
adjustFuncStat fn = do
  (size, sizePQ, sc) <- transactReadOnly $ \schema -> do
    size <- queryJobTree (sizeTree fn) schema
    sizePQ <- queryProcTree (sizeTree fn) schema
    schedAt <- queryJobTree (minSchedAt fn) schema
    return (size, sizePQ, schedAt)

  schedAt <- if sc > 0 then pure sc else liftIO $ getEpochTime

  SchedEnv{..} <- ask
  liftIO $ FL.alter sFuncStatList (update (size + sizePQ) sizePQ schedAt) fn

  where update :: Int64 -> Int64 -> Int64 -> Maybe FuncStat -> Maybe FuncStat
        update size sizePQ schedAt st =
          Just ((fromMaybe (funcStat fn) st) { sJob = size
                                             , sProcess = sizePQ
                                             , sSchedAt = schedAt
                                             })

removeJob :: (MonadIO m, MonadBaseControl IO m, MonadHaskey Schema m) => Job -> SchedT m ()
removeJob job = do
  transact_ $ \schema ->
    updateProcTree (deleteTree fn (hashJobName jn)) schema
      >>= updateJobTree (deleteTree fn jn)
      >>= commit_

  pushChanList (Remove job)
  where jn = jName job
        fn = jFuncName job

dumpJob :: (MonadIO m, MonadHaskey Schema m) => SchedT m [Job]
dumpJob = do
  js <- transactReadOnly $ queryProcTree $ foldrTree (\j acc -> j:acc) []
  js' <- transactReadOnly $ queryJobTree $ foldrTree (\j acc -> j:acc) []
  return . concat $ js ++ js'

alterFunc :: MonadIO m => FuncName -> (Maybe FuncStat -> Maybe FuncStat) -> SchedT m ()
alterFunc n f = do
  SchedEnv{..} <- ask
  liftIO $ FL.alter sFuncStatList f n
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
    when (isJust st) $
      when (sWorker (fromJust st) == 0) $ FL.delete sFuncStatList n

  pushChanList PollJob

pushGrab :: MonadIO m => IOList FuncName -> IOList JobHandle -> AgentEnv' -> SchedT m ()
pushGrab funcList handleList ag = do
  queue <- asks sGrabQueue
  liftIO $ pushAgent queue funcList handleList ag

assignJob :: AgentEnv' -> Job -> IO ()
assignJob env0 job =
  runAgentT' env0 $ send (JobAssign (jHandle job) job)

failJob :: (MonadIO m, MonadBaseControl IO m, MonadHaskey Schema m) => JobHandle -> SchedT m ()
failJob jh = do
  job <- transactReadOnly $ queryProcTree $ lookupJob fn jn
  when (isJust job) $ do
    nextSchedAt <- liftIO getEpochTime
    retryJob ((fromJust job) {jSchedAt = nextSchedAt})

  where (fn, jn) = unHandle jh

retryJob :: (MonadIO m, MonadHaskey Schema m) => Job -> SchedT m ()
retryJob job = do
  transact_ $ \schema -> do
    updateProcTree (deleteTree fn (hashJobName jn)) schema
      >>= updateJobTree (insertTree fn jn job)
      >>= commit_
  pushChanList (Add job)

  where  fn = jFuncName job
         jn = jName job


doneJob :: (MonadIO m, MonadHaskey Schema m) => JobHandle -> SchedT m ()
doneJob jh = do
  transact_ $ \schema -> updateProcTree (deleteTree fn jn) schema >>= commit_

  where (fn, jn) = unHandle jh

schedLaterJob
  :: (MonadIO m, MonadBaseControl IO m, MonadHaskey Schema m)
  => JobHandle -> Int64 -> Int -> SchedT m ()
schedLaterJob jh later step = do
  job <- transactReadOnly $ queryProcTree $ lookupJob fn jn
  when (isJust job) $ do
    let job' = fromJust job

    nextSchedAt <- liftIO $ (+) later <$> getEpochTime
    retryJob job' {jSchedAt = nextSchedAt , jCount = jCount job' + step}

  where (fn, jn) = unHandle jh

status :: (MonadIO m, MonadHaskey Schema m) => SchedT m [FuncStat]
status = do
  mapM_ adjustFuncStat =<< transactReadOnly (queryJobTree treeFuncList)
  liftIO . FL.elems =<< asks sFuncStatList

revertProcessQueue :: (MonadIO m, MonadBaseControl IO m, MonadHaskey Schema m) => SchedT m ()
revertProcessQueue = do
  now <- liftIO getEpochTime
  tout <- liftIO . fmap fromIntegral . readTVarIO =<< asks sTaskTimeout
  handles <- transactReadOnly $ queryProcTree $ foldrTree (foldFunc now tout) []
  mapM_ (failJob . jHandle) $ concat handles

  where foldFunc :: Int64 -> Int64 -> Job -> [Job] -> [Job]
        foldFunc t1 tout job acc | jSchedAt job + tout < t1 = job : acc
                                 | otherwise = acc

shutdown :: (MonadIO m, MonadBaseControl IO m) => SchedT m ()
shutdown = do
  SchedEnv{..} <- ask
  pushChanList Cancel
  alive <- liftIO $ atomically $ do
    t <- readTVar sAlive
    writeTVar sAlive False
    return t
  when alive $ do
    void . async $ liftIO sCleanup
