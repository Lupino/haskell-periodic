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

import           Control.Monad           (unless)
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import           Data.Int                (Int64)
import           Data.Maybe              (fromMaybe)
import           Metro.IOHashMap         (IOHashMap, newIOHashMap)
import qualified Metro.IOHashMap         as IHM
import           Periodic.IOList         (IOList, newIOList)
import qualified Periodic.IOList         as IL
import           Periodic.Server.Persist (Persist (PersistConfig, PersistException),
                                          State (..))
import qualified Periodic.Server.Persist as Persist
import           Periodic.Types.Job      (FuncName (..), Job, JobName (..),
                                          getSchedAt)
import           System.Log.Logger       (infoM)
import           UnliftIO                (Exception, STM, SomeException,
                                          Typeable, atomically)

type JobMap = IOHashMap FuncName (HashMap JobName Job)
type FuncNameList = IOList FuncName

data Memory = Memory
  { pending :: JobMap
  , running :: JobMap
  , locking :: JobMap
  , funcs   :: FuncNameList
  }

instance Persist Memory where
  data PersistConfig Memory = UseMemory
  data PersistException Memory = MemoryException SomeException deriving (Show, Typeable)

  newPersist _ = do
    infoM "Periodic.Server.Persist.Memory" "Memory connected"
    pending <- newIOHashMap
    running <- newIOHashMap
    locking <- newIOHashMap
    funcs   <- newIOList
    return Memory {..}

  member         = doMember
  lookup         = doLookup
  insert         = doInsert
  delete         = doDelete
  size           = doSize
  foldr          = doFoldr
  foldrPending   = doFoldrPending
  foldrLocking   = doFoldrLocking
  dumpJob        = doDumpJob
  configSet      = \_ _ _ -> return ()
  configGet      = \_ _ -> return Nothing
  insertFuncName = doInsertFuncName
  removeFuncName = doRemoveFuncName
  funcList       = doFuncList
  minSchedAt     = doMinSchedAt

instance Exception (PersistException Memory)

useMemory :: PersistConfig Memory
useMemory = UseMemory


getJobMap :: Memory -> State -> JobMap
getJobMap m Pending = pending m
getJobMap m Running = running m
getJobMap m Locking = locking m

getJobMap1 :: Memory -> State -> FuncName -> IO (Maybe (HashMap JobName Job))
getJobMap1 m s = IHM.lookup (getJobMap m s)

doMember :: Memory -> State -> FuncName -> JobName -> IO Bool
doMember m s f j = maybe False (HM.member j) <$> getJobMap1 m s f

doLookup :: Memory -> State -> FuncName -> JobName -> IO (Maybe Job)
doLookup m s f j = maybe Nothing (HM.lookup j) <$> getJobMap1 m s f

deleteJobSTM0 :: JobMap -> FuncName -> JobName -> STM ()
deleteJobSTM0 m f j = do
  mhm <- IHM.lookupSTM m f
  case mhm of
    Nothing -> return ()
    Just hm -> IHM.insertSTM m f (HM.delete j hm)


deleteJobSTM :: Memory -> FuncName -> JobName -> STM ()
deleteJobSTM m f j = do
  deleteJobSTM0 (pending m) f j
  deleteJobSTM0 (running m) f j
  deleteJobSTM0 (locking m) f j


insertJobSTM :: Memory -> State -> FuncName -> JobName -> Job -> STM ()
insertJobSTM m s f j v = do
  hm <- fromMaybe HM.empty <$> IHM.lookupSTM (getJobMap m s) f
  IHM.insertSTM (getJobMap m s) f (HM.insert j v hm)


doInsert :: Memory -> State -> FuncName -> JobName -> Job -> IO ()
doInsert m s f j v = atomically $ do
  deleteJobSTM m f j
  insertJobSTM m s f j v

doDelete :: Memory -> FuncName -> JobName -> IO ()
doDelete m f j = atomically $ deleteJobSTM m f j

doSize :: Memory -> State -> FuncName -> IO Int64
doSize m s f = fromIntegral . maybe 0 HM.size <$> getJobMap1 m s f

doFoldr :: forall a . Memory -> State -> (Job -> a -> a) -> a -> IO a
doFoldr m s f acc =
  atomically $ IHM.foldrWithKeySTM (getJobMap m s) (foldFunc f) acc
  where foldFunc :: (Job -> b -> b) -> FuncName -> HashMap JobName Job -> b -> b
        foldFunc ff _ h acc0 = HM.foldr ff acc0 h

doFoldrPending :: forall a . Memory -> Int64 -> [FuncName] -> (Job -> a -> a) -> a -> IO a
doFoldrPending m st fns f acc =
  atomically $ IHM.foldrWithKeySTM (pending m) (foldFunc f) acc

  where foldFunc :: (Job -> b -> b) -> FuncName -> HashMap JobName Job -> b -> b
        foldFunc ff fn h acc0 | fn `elem` fns = HM.foldr (foldFunc1 ff) acc0 h
                              | otherwise = acc0

        foldFunc1 :: (Job -> c -> c) -> Job -> c -> c
        foldFunc1 ff job acc0 | getSchedAt job < st = ff job acc0
                              | otherwise = acc0

doFoldrLocking :: forall a . Memory -> Int -> FuncName -> (Job -> a -> a) -> a -> IO a
doFoldrLocking m _ fn f acc =
  maybe acc (HM.foldr f acc) <$> getJobMap1 m Locking fn


doDumpJob :: Memory -> IO [Job]
doDumpJob m = do
  hm1 <- IHM.elems (pending m)
  hm2 <- IHM.elems (running m)
  hm3 <- IHM.elems (locking m)
  return $ HM.elems . HM.unions $ concat [hm1, hm2, hm3]


doInsertFuncName :: Memory -> FuncName -> IO ()
doInsertFuncName m fn = atomically $ do
  has <- IL.elemSTM (funcs m) fn
  unless has $ IL.insertSTM (funcs m) fn


doRemoveFuncName :: Memory -> FuncName -> IO ()
doRemoveFuncName m fn = atomically $ do
  IL.deleteSTM (funcs m) fn
  IHM.deleteSTM (pending m) fn
  IHM.deleteSTM (running m) fn
  IHM.deleteSTM (locking m) fn

doFuncList :: Memory -> IO [FuncName]
doFuncList = IL.toList . funcs

safeMinimum :: [Int64] -> Int64
safeMinimum [] = 0
safeMinimum xs = minimum xs

doMinSchedAt :: Memory -> FuncName -> IO Int64
doMinSchedAt m fn =
  maybe 0 (safeMinimum . map getSchedAt . HM.elems) <$> getJobMap1 m Pending fn


jobMapSize :: JobMap -> IO Int64
jobMapSize hm = fromIntegral . sum . map HM.size <$> IHM.elems hm


memorySize :: Memory -> IO Int64
memorySize Memory {..} = do
  s0 <- jobMapSize pending
  s1 <- jobMapSize running
  s2 <- jobMapSize locking

  return $ s0 + s1 + s2
