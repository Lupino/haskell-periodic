{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Periodic.Server.Persist
  ( Persist (..)
  , State (..)
  ) where

import           Control.Exception  (Exception)
import           Data.Int           (Int64)
import           Periodic.Types.Job (FuncName, Job, JobName)
import           Prelude            hiding (foldr, lookup)

data State = Pending
    | Running
    | Locking

class (Exception (PersistException db)) => Persist db where
  data PersistConfig db
  data PersistException db
  newPersist     :: PersistConfig db -> IO db
  member         :: db -> State -> FuncName -> JobName -> IO Bool
  lookup         :: db -> State -> FuncName -> JobName -> IO (Maybe Job)
  insert         :: db -> State -> FuncName -> JobName -> Job -> IO ()
  delete         :: db -> FuncName -> JobName -> IO ()
  size           :: db -> State -> FuncName -> IO Int64
  foldr          :: forall a . db -> State -> (Job -> a -> a) -> a -> IO a
  foldrPending   :: forall a . db -> Int64 -> [FuncName] -> (Job -> a -> a) -> a -> IO a
  getLockedJob   :: forall a . db -> FuncName -> Int -> IO [Job]
  dumpJob        :: db -> IO [Job]
  configGet      :: db -> String -> IO (Maybe Int)
  configSet      :: db -> String -> Int -> IO ()
  insertFuncName :: db -> FuncName -> IO ()
  removeFuncName :: db -> FuncName -> IO ()
  funcList       :: db -> IO [FuncName]
  minSchedAt     :: db -> FuncName -> IO Int64
  countPending   :: forall a . db -> Int64 -> [FuncName] -> IO Int
