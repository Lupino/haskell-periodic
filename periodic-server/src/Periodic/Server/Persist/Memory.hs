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
import           Data.List               (sortOn)
import           Data.Maybe              (catMaybes)
import           Periodic.Server.Persist (Persist (PersistConfig, PersistException),
                                          State (..))
import qualified Periodic.Server.Persist as Persist
import           Periodic.Types.Job      (FuncName (..), Job, JobName (..),
                                          getFuncName, getName, getSchedAt)
import           System.Log.Logger       (infoM)
import           UnliftIO                (Exception, STM, SomeException, TVar,
                                          Typeable, atomically, modifyTVar',
                                          newTVar, readTVar)

data PureJob = PureJob
  { pureState :: State
  , pureJob   :: Job
  }


type JobMap = IOMap JobName (TVar PureJob)

newtype Memory = Memory
  { jobList :: IOMap FuncName JobMap
  }

instance Persist Memory where
  data PersistConfig Memory = UseMemory
  data PersistException Memory = MemoryException SomeException deriving (Show, Typeable)

  newPersist _ = do
    infoM "Periodic.Server.Persist.Memory" "Memory connected"
    jobList  <- IOMap.empty
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
  countPending   = doCountPending
  insertMetric   = doInsertMetric

instance Exception (PersistException Memory)

useMemory :: PersistConfig Memory
useMemory = UseMemory


getJobMap :: Memory -> FuncName -> STM (Maybe JobMap)
getJobMap m = flip IOMapS.lookup (jobList m)

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
  mJobMap <- getJobMap m f
  case mJobMap of
    Nothing     -> do
      pureJobT <- newTVar PureJob
        { pureJob = v
        , pureState = s
        }
      jobMap <- IOMapS.fromList [(j, pureJobT)]
      IOMapS.insert f jobMap (jobList m)
    Just jobMap -> do
      mPureJobT <- IOMapS.lookup j jobMap
      case mPureJobT of
        Nothing -> do
          pureJobT <- newTVar PureJob
            { pureJob = v
            , pureState = s
            }
          IOMapS.insert j pureJobT jobMap
        Just pureJobT ->
          modifyTVar' pureJobT $ \vv -> vv
            { pureJob = v
            , pureState = s
            }
  where f = getFuncName v
        j = getName     v


doUpdateState :: Memory -> State -> FuncName -> JobName -> IO ()
doUpdateState m s f j = atomically $ do
  mJobMap <- getJobMap m f
  case mJobMap of
    Nothing     -> pure ()
    Just jobMap -> do
      mPureJobT <- IOMapS.lookup j jobMap
      case mPureJobT of
        Nothing -> pure ()
        Just pureJobT ->
          modifyTVar' pureJobT $ \vv -> vv
            { pureState = s
            }

doDelete :: Memory -> FuncName -> JobName -> IO ()
doDelete m f j = atomically $ do
  mJobMap <- getJobMap m f
  mapM_ (IOMapS.delete j) mJobMap

doSize :: Memory -> State -> FuncName -> IO Int64
doSize m s f =
  atomically $ sum <$> mapJobMap m f (genMapFunc_ (const 1) (is s))


is :: State -> PureJob -> Bool
is s pj = pureState pj == s


mapJobMap :: Memory -> FuncName -> (TVar PureJob -> STM (Maybe a)) -> STM [a]
mapJobMap m fn mapFunc = do
  mJobMap <- getJobMap m fn
  case mJobMap of
    Nothing     -> pure []
    Just jobMap -> catMaybes <$> (mapM mapFunc =<< IOMapS.elems jobMap)

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

doGetRunningJob :: Memory -> Int64 -> IO [Job]
doGetRunningJob m ts = atomically $ mapPureJob m $ genMapFunc comp
  where comp :: PureJob -> Bool
        comp pj | is Running pj = getSchedAt (pureJob pj) < ts
                | otherwise     = False


takeMin :: Int -> [Job] -> [Job]
takeMin c = take c . sortOn getSchedAt

comparePending :: FuncName -> Int64 -> PureJob -> Bool
comparePending fn0 ts pj
  | isPending && isElem && canSched = True
  | otherwise = False

  where job = pureJob pj
        fn = getFuncName job
        schedAt = getSchedAt job
        isPending = is Pending pj
        isElem = fn0 == fn
        canSched = ts <= 0 || (schedAt < ts)

doGetPendingJob :: Memory -> FuncName -> Int64 -> Int -> IO [Job]
doGetPendingJob m fn ts c =
  atomically $ takeMin c <$> mapPureJob m (genMapFunc (comparePending fn ts))

doGetLockedJob :: Memory -> FuncName -> Int -> IO [Job]
doGetLockedJob m fn c =
  atomically $ takeMin c <$> mapJobMap m fn (genMapFunc (is Locked))

doCountPending :: Memory -> FuncName -> Int64 -> IO Int
doCountPending m fn ts =
  atomically $ sum <$> mapPureJob m (genMapFunc_ (const 1) (comparePending fn ts))

doDumpJob :: Memory -> IO [Job]
doDumpJob m = atomically $ mapPureJob m (genMapFunc $ const True)


doInsertFuncName :: Memory -> FuncName -> IO ()
doInsertFuncName m fn = atomically $ do
  mJobMap <- getJobMap m fn
  case mJobMap of
    Nothing -> do
      jobMap <- IOMapS.empty
      IOMapS.insert fn jobMap (jobList m)
    Just _ -> pure ()


doRemoveFuncName :: Memory -> FuncName -> IO ()
doRemoveFuncName m fn =
  atomically $ IOMapS.delete fn $ jobList m

doFuncList :: Memory -> IO [FuncName]
doFuncList = IOMap.keys . jobList

safeMinimum :: [Int64] -> Int64
safeMinimum [] = 0
safeMinimum xs = minimum xs

doMinSchedAt :: Memory -> FuncName -> IO Int64
doMinSchedAt m fn =
  atomically $ safeMinimum <$> mapJobMap m fn (genMapFunc_ getSchedAt (is Pending))

memorySize :: Memory -> IO Int64
memorySize Memory {..} = do
  sizes <- mapM IOMap.size =<< IOMap.elems jobList
  pure . fromIntegral $ sum sizes

doInsertMetric :: Memory -> String -> String -> Int -> IO ()
doInsertMetric _ _ _ _ = pure ()
