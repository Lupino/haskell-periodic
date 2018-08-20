{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.ClientCommand
  (
    ClientCommand (..)
  ) where

import           Data.Binary
import           Data.Binary.Get         (getWord32be)
import           Data.Binary.Put         (putWord32be)
import           Data.Byteable           (Byteable (..))
import           Data.ByteString.Lazy    (toStrict)
import           Periodic.Types.Internal
import           Periodic.Types.Job      (FuncName, Job, JobName)

data ClientCommand =
    SubmitJob Job
  | Status
  | Ping
  | DropFunc FuncName
  | RemoveJob FuncName JobName
  | ConfigGet ConfigKey
  | ConfigSet ConfigKey Int
  | Dump
  | Load [Job]
  | Shutdown
  | RunJob Job

  deriving (Show)

instance Byteable ClientCommand where
  toBytes = toStrict . encode

instance Parser ClientCommand where
  runParser = parseBinary

instance Binary ClientCommand where
  get = do
    tp <- getWord8
    case tp of
      13 -> SubmitJob <$> get
      14 -> pure Status
      9  -> pure Ping
      15 -> DropFunc <$> get
      17 -> do
        fn <- get
        RemoveJob fn <$> get
      20 -> pure Shutdown
      22 -> ConfigGet <$> get
      23 -> do
        key <- get
        val <- getWord32be
        pure . ConfigSet key $ fromIntegral val
      18 -> pure Dump
      19 -> Load <$> get
      25 -> RunJob <$> get
      _  -> error $ "Error ClientCommand" ++ show tp

  put (SubmitJob job) = do
    putWord8 13
    put job
  put Status          = putWord8 14
  put Ping            = putWord8 9
  put (DropFunc func) = do
    putWord8 15
    put func
  put (RemoveJob fn jn) = do
    putWord8 17
    put fn
    put jn
  put Shutdown        = putWord8 20
  put (ConfigGet key) = do
    putWord8 22
    put key
  put (ConfigSet k v) = do
    putWord8 23
    put k
    putWord32be $ fromIntegral v
  put Dump            = putWord8 18
  put (Load jobs)     = do
    putWord8 19
    put jobs
  put (RunJob job)    = do
    putWord8 25
    put job
