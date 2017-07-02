{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.FuncStat
  (
    FuncStat (..)
  , funcStat
  , FuncStatList
  , getFirstSched
  ) where

import           Data.Int                  (Int64)
import           Periodic.Server.GrabQueue (GrabQueue, hasAgent)
import           Periodic.Server.IOHashMap (IOHashMap, elems)
import           Periodic.Types            (FuncName)

data FuncStat = FuncStat { sSchedAt  :: Int64
                         , sWorker   :: Int64
                         , sJob      :: Int64
                         , sProcess  :: Int64
                         , sFuncName :: FuncName
                         }

type FuncStatList = IOHashMap FuncStat

funcStat :: FuncName -> FuncStat
funcStat sFuncName = FuncStat { sSchedAt = 0, sWorker = 0, sJob = 0, sProcess = 0, .. }

getFirstSched :: FuncStatList -> GrabQueue -> IO (Maybe FuncStat)
getFirstSched sl q = flip go Nothing =<< elems sl

  where go :: [FuncStat] -> Maybe FuncStat -> IO (Maybe FuncStat)
        go [] o     = return o
        go (x:xs) o = do
          let next = do
                has <- hasAgent q (sFuncName x)
                if has then go xs (Just x)
                       else go xs o
          case (o, sWorker x > 0, sJob x - sProcess x > 0) of
                        (_, False, _)         -> go xs o
                        (_, _, False)         -> go xs o
                        (Nothing, _, _) -> next
                        (Just o', _, _) -> if sSchedAt o' > sSchedAt x then next
                                                                       else go xs o
