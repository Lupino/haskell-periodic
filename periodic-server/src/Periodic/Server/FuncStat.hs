{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.FuncStat
  (
    FuncStat (..)
  , funcStat
  , FuncStatList
  ) where

import           Data.Byteable
import qualified Data.ByteString.Char8 as B (intercalate, pack)
import           Data.Int              (Int64)
import           Periodic.IOHashMap    (IOHashMap)
import           Periodic.Types        (FuncName)

data FuncStat = FuncStat { sSchedAt   :: Int64
                         , sWorker    :: Int64
                         , sJob       :: Int64
                         , sProcess   :: Int64
                         , sFuncName  :: FuncName
                         , sBroadcast :: Bool
                         }

instance Byteable FuncStat where
  toBytes FuncStat{..} = B.intercalate ","
    [ toBytes sFuncName
    , B.pack $ show sWorker
    , B.pack $ show sJob
    , B.pack $ show sProcess
    , B.pack $ show sSchedAt
    ]

type FuncStatList = IOHashMap FuncName FuncStat

funcStat :: FuncName -> FuncStat
funcStat sFuncName = FuncStat
  { sSchedAt = 0, sWorker = 0, sJob = 0, sProcess = 0, sBroadcast = False, .. }
