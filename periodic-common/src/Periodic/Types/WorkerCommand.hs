{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.WorkerCommand
  (
    WorkerCommand (..)
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.Int                (Int64)
import           Periodic.Types.Internal
import           Periodic.Types.Job      (FuncName, JobHandle)

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B (length)
import           Data.ByteString.Lazy    (toStrict)

data WorkerCommand =
    GrabJob
  | SchedLater JobHandle Int64 Int
  | WorkDone JobHandle ByteString
  | WorkFail JobHandle
  | Sleep
  | Ping
  | CanDo FuncName
  | CantDo FuncName
  | Broadcast FuncName
  | Acquire LockName Int JobHandle
  | Release LockName JobHandle

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
      3 -> do
        jh <- get
        WorkDone jh . toStrict <$> getRemainingLazyByteString
      4 -> WorkFail <$> get
      11 -> pure Sleep
      9 -> pure Ping
      7 -> CanDo <$> get
      8 -> CantDo <$> get
      21 -> Broadcast <$> get
      27 -> do
        n <- get
        c <- fromIntegral <$> getInt16be
        Acquire n c <$> get
      28 -> do
        n <- get
        Release n <$> get
      _ -> error $ "Error WorkerCommand " ++ show tp

  put GrabJob = putWord8 1
  put (SchedLater jh later step) = do
    putWord8 2
    put jh
    putInt64be later
    putInt16be $ fromIntegral step
  put (WorkDone jh w) = do
    putWord8 3
    put jh
    putByteString w
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
  put (Acquire n c jh) = do
    putWord8 27
    put n
    putInt16be $ fromIntegral c
    put jh
  put (Release n jh)    = do
    putWord8 28
    put n
    put jh

instance Validatable WorkerCommand where
  validate (SchedLater jh _ step) = do
    validate jh
    validateNum "Step" 0 0xFFFF step
  validate (WorkDone jh w) = do
    validate jh
    validateLength "WorkData" 0 0xFFFFFFFF $ B.length w
  validate (WorkFail jh) = validate jh
  validate (CanDo fn) = validate fn
  validate (CantDo fn) = validate fn
  validate (Broadcast fn) = validate fn
  validate (Acquire n c jh) = do
    validate n
    validateNum "LockCount" 1 0xFFFF c
    validate jh
  validate (Release n jh)    = do
    validate n
    validate jh
  validate _               = Right ()
