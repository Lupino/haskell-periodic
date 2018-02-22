{-# LANGUAGE RecordWildCards #-}

module Periodic.Server.FuncStat
  (
    FuncStat (..)
  , funcStat
  , FuncStatList
  ) where

import           Data.Int           (Int64)
import           Periodic.IOHashMap (IOHashMap)
import           Periodic.Types     (FuncName)

data FuncStat = FuncStat { sSchedAt   :: Int64
                         , sWorker    :: Int64
                         , sJob       :: Int64
                         , sProcess   :: Int64
                         , sFuncName  :: FuncName
                         , sBroadcast :: Bool
                         }

type FuncStatList = IOHashMap FuncName FuncStat

funcStat :: FuncName -> FuncStat
funcStat sFuncName = FuncStat
  { sSchedAt = 0, sWorker = 0, sJob = 0, sProcess = 0, sBroadcast = False, .. }
