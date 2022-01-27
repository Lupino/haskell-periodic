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


import           Data.IOMap              (IOMap)
import qualified Data.IOMap              as IOMap
import qualified Data.IOMap.STM          as IOMapS
import           Data.Int                (Int64)
import           Data.List               (sortOn)
import           Data.Maybe              (catMaybes)
import           Periodic.Server.Persist (Persist (PersistConfig, PersistException),
                                          State (..))
import qualified Periodic.Server.Persist as Persist
import           Periodic.Types.Job      (FuncName (..), Job, JobName (..),
                                          getFuncName, getSchedAt)
import           System.Log.Logger       (infoM)
import           UnliftIO                (Exception, STM, SomeException, TVar,
                                          Typeable, atomically, modifyTVar',
                                          newTVar, readTVar)

data MemoryJob = MemoryJob
  { state  :: State
  , memJob :: Job
  }

type JobMap = IOMap FuncName (IOMap JobName (TVar MemoryJob))

newtype Memory = Memory
  { jobList :: JobMap
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
  delete         = doDelete
  size           = doSize
  getRunningJob  = doGetRunningJob
  getPendingJob  = doGetPendingJob
  getLockedJob   = doGetLockedJob
  dumpJob        = doDumpJob
  configSet      = \_ _ _ -> return ()
  configGet      = \_ _ -> return Nothing
  insertFuncName = doInsertFuncName
  removeFuncName = doRemoveFuncName
  funcList       = doFuncList
  minSchedAt     = doMinSchedAt
  countPending   = doCountPending

instance Exception (PersistException Memory)

useMemory :: PersistConfig Memory
useMemory = UseMemory


getJobMap :: Memory -> FuncName -> STM (Maybe (IOMap JobName (TVar MemoryJob)))
getJobMap m = flip IOMapS.lookup (jobList m)

getMemJob :: Memory -> FuncName -> JobName -> STM (Maybe (TVar MemoryJob))
getMemJob m f j = do
  mJobMap <- getJobMap m f
  case mJobMap of
    Nothing     -> pure Nothing
    Just jobMap -> IOMapS.lookup j jobMap

doGetOne :: Memory -> State -> FuncName -> JobName -> IO (Maybe Job)
doGetOne m s f j = atomically $ do
  mMemJobT <- getMemJob m f j
  case mMemJobT of
    Nothing -> pure Nothing
    Just memJobT -> do
      mj <- readTVar memJobT
      if state mj == s then pure . Just $ memJob mj
                       else pure Nothing


doInsert :: Memory -> State -> FuncName -> JobName -> Job -> IO ()
doInsert m s f j v = atomically $ do
  mJobMap <- getJobMap m f
  case mJobMap of
    Nothing     -> do
      memJobT <- newTVar MemoryJob
        { memJob = v
        , state = s
        }
      jobMap <- IOMapS.fromList [(j, memJobT)]
      IOMapS.insert f jobMap (jobList m)
    Just jobMap -> do
      mMemJobT <- IOMapS.lookup j jobMap
      case mMemJobT of
        Nothing -> do
          memJobT <- newTVar MemoryJob
            { memJob = v
            , state = s
            }
          IOMapS.insert j memJobT jobMap
        Just memJobT ->
          modifyTVar' memJobT $ \vv -> vv
            { memJob = v
            , state = s
            }

doDelete :: Memory -> FuncName -> JobName -> IO ()
doDelete m f j = atomically $ do
  mJobMap <- getJobMap m f
  case mJobMap of
    Nothing     -> pure ()
    Just jobMap -> IOMapS.delete j jobMap

doSize :: Memory -> State -> FuncName -> IO Int64
doSize m s f = atomically $ do
  mJobMap <- getJobMap m f
  case mJobMap of
    Nothing     -> pure 0
    Just jobMap -> sum <$> (mapM mapFunc =<< IOMapS.elems jobMap)

  where mapFunc :: TVar MemoryJob -> STM Int64
        mapFunc memJobT = do
          mJob <- readTVar memJobT
          if state mJob == s then pure 1
                             else pure 0

mapMemoryJob :: Memory -> (TVar MemoryJob -> STM (Maybe a)) -> STM [a]
mapMemoryJob m f = do
  memoryJobList <- mapM IOMapS.elems =<< IOMapS.elems (jobList m)
  catMaybes <$> mapM f (concat memoryJobList)


genMapFunc_ :: (Job -> a) -> (MemoryJob -> Bool) -> TVar MemoryJob -> STM (Maybe a)
genMapFunc_ g f h = do
  mj <- readTVar h
  if f mj then pure . Just . g $ memJob mj
          else pure Nothing


genMapFunc :: (MemoryJob -> Bool) -> TVar MemoryJob -> STM (Maybe Job)
genMapFunc = genMapFunc_ id

doGetRunningJob :: Memory -> Int64 -> IO [Job]
doGetRunningJob m ts = atomically $ mapMemoryJob m $ genMapFunc comp
  where comp :: MemoryJob -> Bool
        comp mj | state mj == Running = getSchedAt (memJob mj) < ts
                | otherwise = False


takeMin :: Int -> [Job] -> [Job]
takeMin c = take c . sortOn getSchedAt

comparePending :: [FuncName] -> Int64 -> MemoryJob -> Bool
comparePending fns ts mj
  | isPending && isElem && canSched = True
  | otherwise = False

  where job = memJob mj
        fn = getFuncName job
        schedAt = getSchedAt job
        isPending = state mj == Pending
        isElem = fn `elem` fns
        canSched = ts <= 0 || (schedAt < ts)

doGetPendingJob :: Memory -> [FuncName] -> Int64 -> Int -> IO [Job]
doGetPendingJob m fns ts c =
  atomically $ takeMin c <$> mapMemoryJob m (genMapFunc (comparePending fns ts))

doGetLockedJob :: Memory -> FuncName -> Int -> IO [Job]
doGetLockedJob m fn c = atomically $ do
  mJobMap <- getJobMap m fn
  case mJobMap of
    Nothing -> pure []
    Just jobMap ->
      takeMin c . catMaybes <$> (mapM (genMapFunc comp) =<< IOMapS.elems jobMap)

  where comp :: MemoryJob -> Bool
        comp mj | state mj == Locked = True
                | otherwise = False

doCountPending :: Memory -> [FuncName] -> Int64 -> IO Int
doCountPending m fns ts =
  atomically $ sum <$> mapMemoryJob m (genMapFunc_ f (comparePending fns ts))
  where f :: Job -> Int
        f _ = 1

doDumpJob :: Memory -> IO [Job]
doDumpJob m = atomically $ mapMemoryJob m (genMapFunc $ const True)


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
doMinSchedAt m fn = atomically $ do
  mJobMap <- getJobMap m fn
  case mJobMap of
    Nothing -> pure 0
    Just jobMap ->
      safeMinimum . catMaybes <$> (mapM (genMapFunc_ getSchedAt comp) =<< IOMapS.elems jobMap)

  where comp :: MemoryJob -> Bool
        comp mj = Pending == state mj


memorySize :: Memory -> IO Int64
memorySize Memory {..} = do
  sizes <- mapM IOMap.size =<< IOMap.elems jobList
  pure . fromIntegral $ sum sizes
