{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Periodic.Server.Persist
  ( Persist (..)
  , FuncStats (..)
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

data FuncStats = FuncStats
  { funcPending :: Int64
  , funcRunning :: Int64
  , funcLocked  :: Int64
  , funcSchedAt :: Int64
  } deriving (Show, Eq)

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
  getFuncStats   :: db -> FuncName -> IO FuncStats
  getFuncStats db fn = do
    pending <- size db Pending fn
    running <- size db Running fn
    locked <- size db Locked fn
    schedAt <- minSchedAt db fn
    pure FuncStats
      { funcPending = pending
      , funcRunning = running
      , funcLocked = locked
      , funcSchedAt = schedAt
      }
  countPending   :: db -> FuncName -> Int64 -> IO Int
  insertMetric   :: db -> String -> String -> Int -> IO ()
  insertMetrics  :: db -> [(String, String, Int)] -> IO ()
  insertMetrics db = mapM_ (\(event, name, durationMs) -> insertMetric db event name durationMs)


loopFetchData :: Int -> Int -> Int -> (Int -> IO [a]) -> IO [a]
loopFetchData offset total batchSize doGetBatch = go offset id
  where
    go off build
      | off >= total = pure $ build []
      | otherwise = do
          !batchJobList <- take (total - off) <$> doGetBatch off
          let !batchSize' = length batchJobList
              !build' = build . (batchJobList ++)
          if batchSize' < batchSize
            then pure $ build' []
            else go (off + batchSize) build'
