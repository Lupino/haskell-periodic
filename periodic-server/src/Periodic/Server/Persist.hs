{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}

module Periodic.Server.Persist
  ( Persist (..)
  , persist
  , State (..)
  ) where

import           Prelude            hiding (foldr, lookup)

import           Data.Int           (Int64)
import           Periodic.Types.Job (FuncName, Job, JobName)

data State = Pending | Running | Locking

data Persist = Persist
  { member :: State -> FuncName -> JobName -> IO Bool
  , lookup :: State -> FuncName -> JobName -> IO (Maybe Job)
  , insert :: State -> FuncName -> JobName -> Job -> IO ()
  , delete :: FuncName -> JobName -> IO ()
  , size   :: State -> FuncName -> IO Int64
  , foldr  :: forall a . State -> (Job -> a -> a) -> a -> IO a
  , foldr' :: forall a . State -> [FuncName] -> (Job -> a -> a) -> a -> IO a
  , configGet :: String -> IO (Maybe Int)
  , configSet :: String -> Int -> IO ()
  , insertFuncName   :: FuncName -> IO ()
  , removeFuncName   :: FuncName -> IO ()
  , funcList         :: IO [FuncName]
  , minSchedAt       :: FuncName -> IO Int64
  , transact         :: forall a. IO a -> IO a
  , transactReadOnly :: forall a. IO a -> IO a
  }

persist :: Persist
persist = Persist
  { member = \_ _ _ -> pure False
  , lookup = \_ _ _ -> pure Nothing
  , insert = \_ _ _ _ -> pure ()
  , delete = \_ _ -> pure ()
  , size = \_ _ -> pure 0
  , foldr = \_ _ a -> pure a
  , foldr' = \_ _ _ a -> pure a
  , configGet = \_ -> pure Nothing
  , configSet = \_ _ -> pure ()
  , insertFuncName = \_ -> pure ()
  , removeFuncName = \_ -> pure ()
  , funcList = pure []
  , minSchedAt = \_ -> pure 0
  , transact = id
  , transactReadOnly = id
  }
