{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.ServerCommand
  (
    ServerCommand (..)
  ) where

import           Data.Byteable           (Byteable (..))
import           Periodic.Types.Internal
import           Periodic.Types.Job      (Job, JobHandle)

import           Data.Binary
import           Data.Binary.Get         (getWord32be)
import           Data.Binary.Put         (putWord32be)
import           Data.ByteString.Lazy    (toStrict)

data ServerCommand =
    Noop
  | JobAssign JobHandle Job
  | NoJob
  | Pong
  | Unknown
  | Success
  | Config ConfigKey Int

  deriving (Show)

instance Byteable ServerCommand where
  toBytes = toStrict . encode

instance Parser ServerCommand where
  runParser = parseBinary

instance Binary ServerCommand where
  get = do
    tp <- getWord8
    case tp of
      0 -> pure Noop
      5 -> do
        jh <- get
        job <- get
        pure (JobAssign jh job)
      6 -> pure NoJob
      10 -> pure Pong
      12 -> pure Unknown
      16 -> pure Success
      24 -> do
        key <- get
        val <- getWord32be
        pure . Config key $ fromIntegral val
      _ -> error $ "Error ServerCommand " ++ show tp

  put Noop               = putWord8 0
  put (JobAssign jh job) = do
    putWord8 5
    put jh
    put job
  put NoJob              = putWord8 6
  put Pong               = putWord8 10
  put Unknown            = putWord8 12
  put Success            = putWord8 16
  put (Config k v)       = do
    putWord8 24
    put k
    putWord32be $ fromIntegral v
