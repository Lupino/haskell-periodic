{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.ServerCommand
  (
    ServerCommand (..)
  ) where

import           Data.Byteable           (Byteable (..))
import           Periodic.Types.Internal
import           Periodic.Types.Job      (Job)

import           Data.Binary
import           Data.Binary.Get         (getWord32be)
import           Data.Binary.Put         (putWord32be)
import           Data.ByteString.Lazy    (toStrict)

data ServerCommand =
    Noop
  | JobAssign Job
  | NoJob
  | Pong
  | Unknown
  | Success
  | Config Int
  | Acquired Bool

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
      5 -> JobAssign <$> get
      6 -> pure NoJob
      10 -> pure Pong
      12 -> pure Unknown
      16 -> pure Success
      24 -> do
        val <- getWord32be
        pure . Config $ fromIntegral val
      26 -> do
        v <- getWord8
        pure $ Acquired $ if v == 1 then True else False
      _ -> error $ "Error ServerCommand " ++ show tp

  put Noop            = putWord8 0
  put (JobAssign job) = do
    putWord8 5
    put job
  put NoJob           = putWord8 6
  put Pong            = putWord8 10
  put Unknown         = putWord8 12
  put Success         = putWord8 16
  put (Config v)      = do
    putWord8 24
    putWord32be $ fromIntegral v
  put (Acquired True)    = do
    putWord8 26
    putWord8 1
  put (Acquired False)    = do
    putWord8 26
    putWord8 0
