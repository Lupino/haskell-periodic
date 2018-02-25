{-# LANGUAGE OverloadedStrings #-}

module Periodic.Types.ClientType
  (
    ClientType (..)
  ) where

import           Data.Byteable           (Byteable (..))

import           Data.Binary
import           Data.ByteString.Lazy    (toStrict)
import           Periodic.Types.Internal

data ClientType = TypeClient | TypeWorker
  deriving (Eq, Show)

instance Byteable ClientType where
  toBytes = toStrict . encode

instance Parser ClientType where
  runParser = parseBinary

instance Binary ClientType where
  get = do
    tp <- getWord8
    case tp of
      1 -> pure TypeClient
      2 -> pure TypeWorker
      _ -> error $ "Error ClientType " ++ show tp

  put TypeClient = putWord8 1
  put TypeWorker = putWord8 2
