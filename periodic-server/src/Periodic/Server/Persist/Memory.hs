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
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as HM
import           Data.Maybe              (fromMaybe)
import           Periodic.Server.Persist (Persist (PersistConfig, PersistException),
                                          State (..))
import qualified Periodic.Server.Persist as Persist
import           Periodic.Types.Job      (FuncName (..), Job, JobName (..),
                                          getSchedAt)
import           System.Log.Logger       (infoM)
import           UnliftIO                (Exception, STM, SomeException,
                                          Typeable, atomically)

type JobMap = IOMap FuncName (Map JobName Job)
type FuncNameList = IOMap FuncName ()

data Memory = Memory
  { pending :: JobMap
  , running :: JobMap
  , locked  :: JobMap
  , funcs   :: FuncNameList
  }

instance Persist Memory where
  data PersistConfig Memory = UseMemory
  data PersistException Memory = MemoryException SomeException deriving (Show, Typeable)

  newPersist _ = do
    infoM "Periodic.Server.Persist.Memory" "Memory connected"
    pending <- IOMap.empty
    running <- IOMap.empty
    locked  <- IOMap.empty
    funcs   <- IOMap.empty
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


getJobMap :: Memory -> State -> JobMap
getJobMap m Pending = pending m
getJobMap m Running = running m
getJobMap m Locked  = locked m

getJobMap1 :: Memory -> State -> FuncName -> IO (Maybe (Map JobName Job))
getJobMap1 m s = flip IOMap.lookup (getJobMap m s)

doGetOne :: Memory -> State -> FuncName -> JobName -> IO (Maybe Job)
doGetOne m s f j = maybe Nothing (HM.lookup j) <$> getJobMap1 m s f

deleteJobSTM0 :: JobMap -> FuncName -> JobName -> STM ()
deleteJobSTM0 m f j = do
  mhm <- IOMapS.lookup f m
  case mhm of
    Nothing -> return ()
    Just hm -> IOMapS.insert f (HM.delete j hm) m


deleteJobSTM :: Memory -> FuncName -> JobName -> STM ()
deleteJobSTM m f j = do
  deleteJobSTM0 (pending m) f j
  deleteJobSTM0 (running m) f j
  deleteJobSTM0 (locked m) f j


insertJobSTM :: Memory -> State -> FuncName -> JobName -> Job -> STM ()
insertJobSTM m s f j v = do
  hm <- fromMaybe HM.empty <$> IOMapS.lookup f (getJobMap m s)
  IOMapS.insert f (HM.insert j v hm) (getJobMap m s)


doInsert :: Memory -> State -> FuncName -> JobName -> Job -> IO ()
doInsert m s f j v = atomically $ do
  deleteJobSTM m f j
  insertJobSTM m s f j v

doDelete :: Memory -> FuncName -> JobName -> IO ()
doDelete m f j = atomically $ deleteJobSTM m f j

doSize :: Memory -> State -> FuncName -> IO Int64
doSize m s f = fromIntegral . maybe 0 HM.size <$> getJobMap1 m s f

doGetRunningJob :: Memory -> Int64 -> IO [Job]
doGetRunningJob m ts =
  filter ((< ts) . getSchedAt)
    <$> IOMap.foldrWithKey foldFunc [] (getJobMap m Running)
  where foldFunc :: FuncName -> Map JobName Job -> [Job] -> [Job]
        foldFunc _ h acc0 = HM.foldr (:) acc0 h

takeMin :: Int -> [Job] -> [Job]
takeMin c = take c . sortOn getSchedAt

doGetPendingJob :: Memory -> [FuncName] -> Int64 -> Int -> IO [Job]
doGetPendingJob m fns ts c =
  takeMin c <$> IOMap.foldrWithKey foldFunc [] (pending m)

  where foldFunc :: FuncName -> Map JobName Job -> [Job] -> [Job]
        foldFunc fn h acc0 | fn `elem` fns = HM.foldr foldFunc1 acc0 h
                           | otherwise = acc0

        foldFunc1 :: Job -> [Job] -> [Job]
        foldFunc1 job acc0 | getSchedAt job < ts = job : acc0
                           | otherwise = acc0

doGetLockedJob :: Memory -> FuncName -> Int -> IO [Job]
doGetLockedJob m fn c = takeMin c . maybe [] HM.elems <$> getJobMap1 m Locked fn

doCountPending :: Memory -> [FuncName] -> Int64 -> IO Int
doCountPending m fns ts =
  IOMap.foldrWithKey foldFunc 0 (pending m)

  where foldFunc :: FuncName -> Map JobName Job -> Int -> Int
        foldFunc fn h acc | fn `elem` fns = HM.foldr foldFunc1 acc h
                          | otherwise = acc

        foldFunc1 :: Job -> Int -> Int
        foldFunc1 job acc | getSchedAt job < ts = acc + 1
                          | otherwise = acc


doDumpJob :: Memory -> IO [Job]
doDumpJob m = do
  hm1 <- IOMap.elems (pending m)
  hm2 <- IOMap.elems (running m)
  hm3 <- IOMap.elems (locked m)
  return $ HM.elems . HM.unions $ concat [hm1, hm2, hm3]


doInsertFuncName :: Memory -> FuncName -> IO ()
doInsertFuncName m fn = IOMap.insert fn () $ funcs m


doRemoveFuncName :: Memory -> FuncName -> IO ()
doRemoveFuncName m fn = atomically $ do
  IOMapS.delete fn $ funcs m
  IOMapS.delete fn $ pending m
  IOMapS.delete fn $ running m
  IOMapS.delete fn $ locked m

doFuncList :: Memory -> IO [FuncName]
doFuncList = IOMap.keys . funcs

safeMinimum :: [Int64] -> Int64
safeMinimum [] = 0
safeMinimum xs = minimum xs

doMinSchedAt :: Memory -> FuncName -> IO Int64
doMinSchedAt m fn =
  maybe 0 (safeMinimum . map getSchedAt . HM.elems) <$> getJobMap1 m Pending fn


jobMapSize :: JobMap -> IO Int64
jobMapSize hm = fromIntegral . sum . map HM.size <$> IOMap.elems hm


memorySize :: Memory -> IO Int64
memorySize Memory {..} = do
  s0 <- jobMapSize pending
  s1 <- jobMapSize running
  s2 <- jobMapSize locked

  return $ s0 + s1 + s2
