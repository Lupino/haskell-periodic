{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}

module Periodic.Server.Persist
  ( Persister (..)
  , persister
  , Persist (..)
  , persist
  ) where

import           Prelude            hiding (foldr, lookup)

import           Data.Byteable      (Byteable (..))
import           Data.Int           (Int64)
import           Periodic.Types.Job (FuncName, Job)

data Persister = Persister
  { member     :: forall k . Byteable k => FuncName -> k -> IO Bool
  , lookup     :: forall k . Byteable k => FuncName -> k -> IO (Maybe Job)
  , insert     :: forall k . Byteable k => FuncName -> k -> Job -> IO ()
  , delete     :: forall k . Byteable k => FuncName -> k -> IO ()
  , size       :: FuncName -> IO Int64
  , minSchedAt :: FuncName -> IO Int64
  , funcList   :: IO [FuncName]
  , foldr      :: forall a . (Job -> a -> a) -> a -> IO a
  , foldr'     :: forall a . [FuncName] -> (Job -> a -> a) -> a -> IO a
  }

persister :: Persister
persister = Persister
  { member = \_ _ -> pure False
  , lookup = \_ _ -> pure Nothing
  , insert = \_ _ _ -> pure ()
  , delete = \_ _ -> pure ()
  , size = \_ -> pure 0
  , minSchedAt = \_ -> pure 0
  , funcList = pure []
  , foldr = \_ a -> pure a
  , foldr' = \_ _ a -> pure a
  }

data Persist = Persist
  { proc             :: Persister
  , main             :: Persister
  , transact         :: forall a. IO a -> IO a
  , transactReadOnly :: forall a. IO a -> IO a
  }

persist :: Persist
persist = Persist
  { proc = persister
  , main = persister
  , transact = id
  , transactReadOnly = id
  }
