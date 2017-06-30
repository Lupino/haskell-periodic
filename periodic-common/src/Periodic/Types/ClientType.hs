module Periodic.Types.ClientType
  (
    ClientType (..)
  ) where

import           GHC.Enum (boundedEnumFrom, boundedEnumFromThen)

data ClientType = TypeClient | TypeWorker
  deriving (Eq, Show)

instance Bounded ClientType where
  minBound = TypeClient
  maxBound = TypeWorker

instance Enum ClientType where
  succ TypeClient = TypeWorker
  succ TypeWorker = errorWithoutStackTrace "Types.ClientType.succ: bad argument"

  pred TypeWorker = TypeClient
  pred TypeClient = errorWithoutStackTrace "Types.ClientType.pred: bad argument"

  toEnum n | n == 1  = TypeClient
           | n == 2  = TypeWorker
  toEnum _ = errorWithoutStackTrace "Types.ClientType.toEnum: bad argument"

  fromEnum TypeClient = 1
  fromEnum TypeWorker = 2

  -- Use defaults for the rest
  enumFrom     = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
