{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Periodic.Server.Persist.Memory
  ( Memory
  , useMemory
  , memorySize
  ) where


import           Data.Int                (Int64)
import           Data.IOMap              (IOMap)
import qualified Data.IOMap              as IOMap
import qualified Data.IOMap.STM          as IOMapS
import qualified Data.Map.Strict         as Map
import           Data.Map.Strict         (Map)
import           Data.Maybe              (catMaybes)
import           Periodic.Server.Persist (Persist (..),
                                          FuncStats (..),
                                          State (..))
import           Periodic.Types.Job      (FuncName (..), Job, JobName (..),
                                          getFuncName, getName, getSchedAt)
import           System.Log.Logger       (infoM)
import           UnliftIO                (Exception, STM, SomeException, TVar,
                                          Typeable, atomically, modifyTVar',
                                          newTVar, newTVarIO, readTVar,
                                          readTVarIO, writeTVar)

data PureJob = PureJob
  { pureState :: State
  , pureJob   :: Job
  }


type JobMap = IOMap JobName (TVar PureJob)
type SchedIndex = Map (Int64, JobName) Job

data FuncIndex = FuncIndex
  { idxPending      :: SchedIndex
  , idxRunning      :: SchedIndex
  , idxLocked       :: SchedIndex
  , idxPendingCount :: Int64
  , idxRunningCount :: Int64
  , idxLockedCount  :: Int64
  }

data Memory = Memory
  { jobList :: IOMap FuncName JobMap
  , jobIndex :: IOMap FuncName (TVar FuncIndex)
  , jobSize :: TVar Int64
  }

instance Persist Memory where
  data PersistConfig Memory = UseMemory
  data PersistException Memory = MemoryException SomeException deriving (Show, Typeable)

  newPersist _ = do
    infoM "Periodic.Server.Persist.Memory" "Memory connected"
    jobList   <- IOMap.empty
    jobIndex  <- IOMap.empty
    jobSize   <- newTVarIO 0
    return Memory {..}

  getOne         = doGetOne
  insert         = doInsert
  updateState    = doUpdateState
  delete         = doDelete
  size           = doSize
  getRunningJob  = doGetRunningJob
  getPendingJob  = doGetPendingJob
  getLockedJob   = doGetLockedJob
  dumpJob        = doDumpJob
  configSet _ _ _= return ()
  configGet _ _  = return Nothing
  insertFuncName = doInsertFuncName
  removeFuncName = doRemoveFuncName
  funcList       = doFuncList
  minSchedAt     = doMinSchedAt
  getFuncStats   = doGetFuncStats
  countPending   = doCountPending
  insertMetric   = doInsertMetric
  insertMetrics _ _ = pure ()

instance Exception (PersistException Memory)

useMemory :: PersistConfig Memory
useMemory = UseMemory

emptyFuncIndex :: FuncIndex
emptyFuncIndex = FuncIndex
  { idxPending = Map.empty
  , idxRunning = Map.empty
  , idxLocked = Map.empty
  , idxPendingCount = 0
  , idxRunningCount = 0
  , idxLockedCount = 0
  }


getJobMap :: Memory -> FuncName -> STM (Maybe JobMap)
getJobMap m = flip IOMapS.lookup (jobList m)

getFuncIndex :: Memory -> FuncName -> STM (Maybe (TVar FuncIndex))
getFuncIndex m = flip IOMapS.lookup (jobIndex m)

ensureJobMap :: Memory -> FuncName -> STM JobMap
ensureJobMap m fn = do
  mJobMap <- getJobMap m fn
  case mJobMap of
    Just jobMap -> pure jobMap
    Nothing -> do
      jobMap <- IOMapS.empty
      IOMapS.insert fn jobMap (jobList m)
      pure jobMap

ensureFuncIndex :: Memory -> FuncName -> STM (TVar FuncIndex)
ensureFuncIndex m fn = do
  mIndex <- getFuncIndex m fn
  case mIndex of
    Just index -> pure index
    Nothing -> do
      index <- newTVar emptyFuncIndex
      IOMapS.insert fn index (jobIndex m)
      pure index

getPureJob :: Memory -> FuncName -> JobName -> STM (Maybe (TVar PureJob))
getPureJob m f j = do
  mJobMap <- getJobMap m f
  case mJobMap of
    Nothing     -> pure Nothing
    Just jobMap -> IOMapS.lookup j jobMap

doGetOne :: Memory -> State -> FuncName -> JobName -> IO (Maybe Job)
doGetOne m s f j = atomically $ do
  mPureJobT <- getPureJob m f j
  case mPureJobT of
    Nothing -> pure Nothing
    Just pureJobT -> do
      pj <- readTVar pureJobT
      if is s pj then pure . Just $ pureJob pj
                 else pure Nothing


doInsert :: Memory -> State -> Job -> IO ()
doInsert m s v = atomically $ do
  jobMap <- ensureJobMap m f
  indexT <- ensureFuncIndex m f
  mPureJobT <- IOMapS.lookup j jobMap
  case mPureJobT of
    Nothing -> do
      pureJobT <- newTVar PureJob
        { pureJob = v
        , pureState = s
        }
      IOMapS.insert j pureJobT jobMap
      modifyTVar' indexT $ insertIndex s v
      modifyTVar' (jobSize m) (+1)
    Just pureJobT -> do
      old <- readTVar pureJobT
      writeTVar pureJobT PureJob
        { pureJob = v
        , pureState = s
        }
      modifyTVar' indexT $ insertIndex s v . deleteIndex (pureState old) (pureJob old)
  where f = getFuncName v
        j = getName     v


doUpdateState :: Memory -> State -> FuncName -> JobName -> IO ()
doUpdateState m s f j = atomically $ do
  mJobMap <- getJobMap m f
  mIndexT <- getFuncIndex m f
  case mJobMap of
    Nothing     -> pure ()
    Just jobMap -> do
      mPureJobT <- IOMapS.lookup j jobMap
      case mPureJobT of
        Nothing -> pure ()
        Just pureJobT -> do
          old <- readTVar pureJobT
          writeTVar pureJobT old { pureState = s }
          case mIndexT of
            Nothing -> pure ()
            Just indexT ->
              modifyTVar' indexT $ insertIndex s (pureJob old) . deleteIndex (pureState old) (pureJob old)

doDelete :: Memory -> FuncName -> JobName -> IO ()
doDelete m f j = atomically $ do
  mJobMap <- getJobMap m f
  mIndexT <- getFuncIndex m f
  case mJobMap of
    Nothing -> pure ()
    Just jobMap -> do
      mPureJobT <- IOMapS.lookup j jobMap
      case mPureJobT of
        Nothing -> pure ()
        Just pureJobT -> do
          old <- readTVar pureJobT
          IOMapS.delete j jobMap
          case mIndexT of
            Nothing     -> pure ()
            Just indexT -> modifyTVar' indexT $ deleteIndex (pureState old) (pureJob old)
          modifyTVar' (jobSize m) (max 0 . subtract 1)

doSize :: Memory -> State -> FuncName -> IO Int64
doSize m s f = atomically $ do
  mIndexT <- getFuncIndex m f
  case mIndexT of
    Nothing     -> pure 0
    Just indexT -> stateCount s <$> readTVar indexT


is :: State -> PureJob -> Bool
is s pj = pureState pj == s


mapPureJob :: Memory -> (TVar PureJob -> STM (Maybe a)) -> STM [a]
mapPureJob m f = do
  memoryJobList <- mapM IOMapS.elems =<< IOMapS.elems (jobList m)
  catMaybes <$> mapM f (concat memoryJobList)


genMapFunc_ :: (Job -> a) -> (PureJob -> Bool) -> TVar PureJob -> STM (Maybe a)
genMapFunc_ g f h = do
  pj <- readTVar h
  if f pj then pure . Just . g $ pureJob pj
          else pure Nothing


genMapFunc :: (PureJob -> Bool) -> TVar PureJob -> STM (Maybe Job)
genMapFunc = genMapFunc_ id

jobIndexKey :: Job -> (Int64, JobName)
jobIndexKey job = (getSchedAt job, getName job)

insertIndex :: State -> Job -> FuncIndex -> FuncIndex
insertIndex Pending job idx = idx
  { idxPending = Map.insert (jobIndexKey job) job (idxPending idx)
  , idxPendingCount = idxPendingCount idx + 1
  }
insertIndex Running job idx = idx
  { idxRunning = Map.insert (jobIndexKey job) job (idxRunning idx)
  , idxRunningCount = idxRunningCount idx + 1
  }
insertIndex Locked job idx = idx
  { idxLocked = Map.insert (jobIndexKey job) job (idxLocked idx)
  , idxLockedCount = idxLockedCount idx + 1
  }

deleteIndex :: State -> Job -> FuncIndex -> FuncIndex
deleteIndex Pending job idx = idx
  { idxPending = Map.delete (jobIndexKey job) (idxPending idx)
  , idxPendingCount = max 0 $ idxPendingCount idx - 1
  }
deleteIndex Running job idx = idx
  { idxRunning = Map.delete (jobIndexKey job) (idxRunning idx)
  , idxRunningCount = max 0 $ idxRunningCount idx - 1
  }
deleteIndex Locked job idx = idx
  { idxLocked = Map.delete (jobIndexKey job) (idxLocked idx)
  , idxLockedCount = max 0 $ idxLockedCount idx - 1
  }

stateCount :: State -> FuncIndex -> Int64
stateCount Pending = idxPendingCount
stateCount Running = idxRunningCount
stateCount Locked  = idxLockedCount

jobsBefore :: Int64 -> Int -> SchedIndex -> [Job]
jobsBefore ts c jobs
  | c <= 0 = []
  | ts <= 0 = take c $ Map.elems jobs
  | otherwise = take c $ Map.elems $ fst $ Map.split (ts, JobName mempty) jobs

jobCountBefore :: Int64 -> SchedIndex -> Int
jobCountBefore ts jobs
  | ts <= 0 = Map.size jobs
  | otherwise = Map.size $ fst $ Map.split (ts, JobName mempty) jobs

minSchedAtIndex :: SchedIndex -> Int64
minSchedAtIndex jobs =
  case Map.lookupMin jobs of
    Nothing -> 0
    Just ((schedAt, _), _) -> schedAt

funcIndexJobsBefore :: Int64 -> FuncIndex -> [Job]
funcIndexJobsBefore ts = jobsBefore ts maxBound . idxRunning

doGetRunningJob :: Memory -> Int64 -> IO [Job]
doGetRunningJob m ts = atomically $ do
  indexes <- mapM readTVar =<< IOMapS.elems (jobIndex m)
  pure $ concatMap (funcIndexJobsBefore ts) indexes

doGetPendingJob :: Memory -> FuncName -> Int64 -> Int -> IO [Job]
doGetPendingJob m fn ts c = atomically $ do
  mIndexT <- getFuncIndex m fn
  case mIndexT of
    Nothing     -> pure []
    Just indexT -> jobsBefore ts c . idxPending <$> readTVar indexT

doGetLockedJob :: Memory -> FuncName -> Int -> IO [Job]
doGetLockedJob m fn c = atomically $ do
  mIndexT <- getFuncIndex m fn
  case mIndexT of
    Nothing     -> pure []
    Just indexT -> jobsBefore 0 c . idxLocked <$> readTVar indexT

doCountPending :: Memory -> FuncName -> Int64 -> IO Int
doCountPending m fn ts = atomically $ do
  mIndexT <- getFuncIndex m fn
  case mIndexT of
    Nothing     -> pure 0
    Just indexT -> jobCountBefore ts . idxPending <$> readTVar indexT

doDumpJob :: Memory -> IO [Job]
doDumpJob m = atomically $ mapPureJob m (genMapFunc $ const True)


doInsertFuncName :: Memory -> FuncName -> IO ()
doInsertFuncName m fn = atomically $ do
  _ <- ensureJobMap m fn
  _ <- ensureFuncIndex m fn
  pure ()


doRemoveFuncName :: Memory -> FuncName -> IO ()
doRemoveFuncName m fn = atomically $ do
  mJobMap <- getJobMap m fn
  mIndexT <- getFuncIndex m fn
  case mJobMap of
    Nothing -> IOMapS.delete fn $ jobIndex m
    Just _ -> do
      count <- case mIndexT of
        Nothing -> pure 0
        Just indexT -> do
          index <- readTVar indexT
          pure $ idxPendingCount index + idxRunningCount index + idxLockedCount index
      modifyTVar' (jobSize m) (max 0 . subtract count)
      IOMapS.delete fn $ jobList m
      IOMapS.delete fn $ jobIndex m

doFuncList :: Memory -> IO [FuncName]
doFuncList = IOMap.keys . jobList

doMinSchedAt :: Memory -> FuncName -> IO Int64
doMinSchedAt m fn = atomically $ do
  mIndexT <- getFuncIndex m fn
  case mIndexT of
    Nothing     -> pure 0
    Just indexT -> minSchedAtIndex . idxPending <$> readTVar indexT

doGetFuncStats :: Memory -> FuncName -> IO FuncStats
doGetFuncStats m fn = atomically $ do
  mIndexT <- getFuncIndex m fn
  case mIndexT of
    Nothing -> pure $ FuncStats 0 0 0 0
    Just indexT -> do
      index <- readTVar indexT
      pure FuncStats
        { funcPending = idxPendingCount index
        , funcRunning = idxRunningCount index
        , funcLocked = idxLockedCount index
        , funcSchedAt = minSchedAtIndex $ idxPending index
        }

memorySize :: Memory -> IO Int64
memorySize Memory {..} = readTVarIO jobSize

doInsertMetric :: Memory -> String -> String -> Int -> IO ()
doInsertMetric _ _ _ _ = pure ()
