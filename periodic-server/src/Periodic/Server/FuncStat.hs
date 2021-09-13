{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.FuncStat
  ( FuncStat (..)
  , funcStat
  , FuncStatList
  ) where

import qualified Data.ByteString.Char8 as B (intercalate, pack)
import           Data.Byteable
import           Data.IOMap            (IOMap)
import           Data.Int              (Int64)
import           Periodic.Types        (FuncName (..))

data FuncStat = FuncStat
    { sSchedAt   :: Int64
    , sWorker    :: Int64
    , sJob       :: Int64
    , sRunning   :: Int64
    , sLocked    :: Int64
    , sFuncName  :: FuncName
    , sBroadcast :: Bool
    }

instance Byteable FuncStat where
  toBytes FuncStat{..} = B.intercalate ","
    [ unFN sFuncName
    , B.pack $ show sWorker
    , B.pack $ show sJob
    , B.pack $ show sRunning
    , B.pack $ show sLocked
    , B.pack $ show sSchedAt
    ]

type FuncStatList = IOMap FuncName FuncStat

funcStat :: FuncName -> FuncStat
funcStat sFuncName = FuncStat
  { sSchedAt   = 0
  , sWorker    = 0
  , sJob       = 0
  , sRunning   = 0
  , sLocked    = 0
  , sBroadcast = False
  , ..
  }
