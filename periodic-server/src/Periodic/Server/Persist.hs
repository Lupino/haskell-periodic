{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Periodic.Server.Persist
  ( Persist (..)
  , State (..)
  , loopFetchData
  ) where

import           Control.Exception  (Exception)
import           Data.Int           (Int64)
import           Periodic.Types.Job (FuncName, Job, JobName)
import           Prelude            hiding (foldr, lookup)

data State = Pending
    | Running
    | Locked
    deriving (Show, Eq)

class (Exception (PersistException db)) => Persist db where
  data PersistConfig db
  data PersistException db
  newPersist     :: PersistConfig db -> IO db
  getOne         :: db -> State -> FuncName -> JobName -> IO (Maybe Job)
  insert         :: db -> State -> Job -> IO ()
  updateState    :: db -> State -> FuncName -> JobName -> IO ()
  delete         :: db -> FuncName -> JobName -> IO ()
  size           :: db -> State -> FuncName -> IO Int64
  getRunningJob  :: db -> Int64 -> IO [Job]
  getPendingJob  :: db -> FuncName -> Int64 -> Int -> IO [Job]
  getLockedJob   :: db -> FuncName -> Int -> IO [Job]
  dumpJob        :: db -> IO [Job]
  configGet      :: db -> String -> IO (Maybe Int)
  configSet      :: db -> String -> Int -> IO ()
  insertFuncName :: db -> FuncName -> IO ()
  removeFuncName :: db -> FuncName -> IO ()
  funcList       :: db -> IO [FuncName]
  minSchedAt     :: db -> FuncName -> IO Int64
  countPending   :: db -> FuncName -> Int64 -> IO Int
  insertMetric   :: db -> String -> String -> Int -> IO ()


loopFetchData :: Int -> Int -> Int -> (Int -> IO [a]) -> IO [a]
loopFetchData offset total batchSize doGetBatch
  | offset > total = pure []
  | otherwise = do
  !batchJobList <- doGetBatch offset
  if length batchJobList < batchSize then pure batchJobList
  else do
    !jobList <- loopFetchData (offset + batchSize) total batchSize doGetBatch
    pure $! jobList ++ batchJobList
