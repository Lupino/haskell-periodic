{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.ClientCommand
  (
    ClientCommand (..)
  ) where

import           Data.Binary
import           Data.Byteable           (Byteable (..))
import           Data.ByteString.Lazy    (toStrict)
import           Periodic.Types.Internal
import           Periodic.Types.Job      (FuncName, Job)
import           Prelude                 hiding (take)

data ClientCommand =
    SubmitJob Job
  | Status
  | Ping
  | DropFunc FuncName
  | RemoveJob Job
  | Shutdown

  deriving (Show)

instance Byteable ClientCommand where
  toBytes = toStrict . encode

instance Parser ClientCommand where
  runParser = parseBinary

instance Binary ClientCommand where
  get = do
    tp <- getWord8
    case tp of
      13 -> do
        job <- get
        pure (SubmitJob job)
      14 -> pure Status
      9 -> pure Ping
      15 -> do
        func <- get
        pure (DropFunc func)
      17 -> do
        job <- get
        pure (RemoveJob job)
      20 -> pure Shutdown
      _ -> error $ "Error ClientCommand" ++ show tp

  put (SubmitJob job) = do
    putWord8 13
    put job
  put Status          = putWord8 14
  put Ping            = putWord8 9
  put (DropFunc func) = do
    putWord8 15
    put func
  put (RemoveJob job) = do
    putWord8 17
    put job
  put Shutdown        = putWord8 20
