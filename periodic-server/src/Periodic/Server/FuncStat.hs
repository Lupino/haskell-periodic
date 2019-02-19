{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.FuncStat
  (
    FuncStat (..)
  , funcStat
  , FuncStatList
  ) where

import           Data.Byteable
import qualified Data.ByteString.Char8 as B (drop, intercalate, pack)
import           Data.Int              (Int64)
import           Periodic.IOHashMap    (IOHashMap)
import           Periodic.Types        (FuncName)

data FuncStat = FuncStat { sSchedAt   :: Int64
                         , sWorker    :: Int64
                         , sJob       :: Int64
                         , sRunning   :: Int64
                         , sLocking   :: Int64
                         , sFuncName  :: FuncName
                         , sBroadcast :: Bool
                         }

instance Byteable FuncStat where
  toBytes FuncStat{..} = B.intercalate ","
    [ B.drop 1 $ toBytes sFuncName
    , B.pack $ show sWorker
    , B.pack $ show sJob
    , B.pack $ show sRunning
    , B.pack $ show sLocking
    , B.pack $ show sSchedAt
    ]

type FuncStatList = IOHashMap FuncName FuncStat

funcStat :: FuncName -> FuncStat
funcStat sFuncName = FuncStat
  { sSchedAt = 0, sWorker = 0, sJob = 0, sRunning = 0, sLocking = 0, sBroadcast = False, .. }
