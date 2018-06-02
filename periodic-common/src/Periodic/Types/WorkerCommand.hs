{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.WorkerCommand
  (
    WorkerCommand (..)
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.Int                (Int64)
import           Periodic.Types.Internal
import           Periodic.Types.Job      (FuncName, JobHandle, Workload)

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString         (ByteString)
import           Data.ByteString.Lazy    (toStrict)

data WorkerCommand =
    GrabJob
  | SchedLater JobHandle Int64 Int
  | WorkDone JobHandle
  | WorkFail JobHandle
  | WorkData JobHandle ByteString
  | Sleep
  | Ping
  | CanDo FuncName
  | CantDo FuncName
  | Broadcast FuncName

  deriving (Show)

instance Byteable WorkerCommand where
  toBytes = toStrict . encode

instance Parser WorkerCommand where
  runParser = parseBinary

instance Binary WorkerCommand where
  get = do
    tp <- getWord8
    case tp of
      1 -> pure GrabJob
      2 -> do
        jh <- get
        later <- getInt64be
        step <- fromIntegral <$> getInt16be
        pure (SchedLater jh later step)
      3 -> WorkDone <$> get
      4 -> WorkFail <$> get
      11 -> pure Sleep
      9 -> pure Ping
      7 -> CanDo <$> get
      8 -> CantDo <$> get
      21 -> Broadcast <$> get
      30 -> do
        jh <- get
        WorkData jh . toStrict <$> getRemainingLazyByteString
      _ -> error $ "Error WorkerCommand " ++ show tp

  put GrabJob = putWord8 1
  put (SchedLater jh later step) = do
    putWord8 2
    put jh
    putInt64be later
    putInt16be $ fromIntegral step
  put (WorkDone jh) = do
    putWord8 3
    put jh
  put (WorkFail jh) = do
    putWord8 4
    put jh
  put Sleep = putWord8 11
  put Ping = putWord8 9
  put (CanDo fn) = do
    putWord8 7
    put fn
  put (CantDo fn) = do
    putWord8 8
    put fn
  put (Broadcast fn) = do
    putWord8 21
    put fn
  put (WorkData jh w) = do
    putWord8 30
    put jh
    putByteString w
