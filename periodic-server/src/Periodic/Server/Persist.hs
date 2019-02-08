{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Periodic.Server.Persist
  ( Persist (..)
  , persist
  , State (..)
  , stateName
  , stateName'
  ) where

import           Prelude            hiding (foldr, lookup)

import           Data.Byteable      (Byteable (..))
import           Data.ByteString    (ByteString)
import           Data.Int           (Int64)
import           Periodic.Types.Job (FuncName, Job)

data State = Pending | Running

stateName :: State -> ByteString
stateName Pending = "0"
stateName Running = "1"

stateName' :: State -> Int64
stateName' Pending = 0
stateName' Running = 1

data Persist = Persist
  { member :: forall k . Byteable k => State -> FuncName -> k -> IO Bool
  , lookup :: forall k . Byteable k => State -> FuncName -> k -> IO (Maybe Job)
  , insert :: forall k . Byteable k => State -> FuncName -> k -> Job -> IO ()
  , delete :: forall k . Byteable k => FuncName -> k -> IO ()
  , size   :: State -> FuncName -> IO Int64
  , foldr  :: forall a . State -> (Job -> a -> a) -> a -> IO a
  , foldr' :: forall a . State -> [FuncName] -> (Job -> a -> a) -> a -> IO a
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
  , insertFuncName = \_ -> pure ()
  , removeFuncName = \_ -> pure ()
  , funcList = pure []
  , minSchedAt = \_ -> pure 0
  , transact = id
  , transactReadOnly = id
  }
