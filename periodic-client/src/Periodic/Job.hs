{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Job
  (
    Job (..)
  , newJob
  , workDone
  , workFail
  , schedLater
  , schedLater'
  ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (breakSubstring, concat, drop, pack)
import           Periodic.Agent        (send)
import           Periodic.BaseClient   (BaseClient, withAgent)
import           Periodic.Types        (Command (..), nullChar)
import qualified Periodic.Types.Job    as J

import           Data.Int              (Int64)

data Job = Job { name     :: J.JobName
               , func     :: J.FuncName
               , workload :: J.Workload
               , counter  :: Int
               , bc       :: BaseClient
               , handle   :: J.JobHandle
               }

newJob :: BaseClient -> ByteString -> Maybe Job
newJob c = parse . B.breakSubstring nullChar
  where parse :: (ByteString, ByteString) -> Maybe Job
        parse (h, dat) = parse1 h (J.parseJob $ B.drop 2 dat)

        parse1 :: ByteString -> Maybe J.Job -> Maybe Job
        parse1 _ Nothing = Nothing
        parse1 h (Just r) = Just Job { name = J.jName r
                                     , func = J.jFuncName r
                                     , workload = J.jWorkload r
                                     , counter = J.jCount r
                                     , handle = h
                                     , bc = c
                                     }


workDone :: Job -> IO ()
workDone (Job { handle = h, bc = c }) = withAgent c $ \agent ->
  send agent WorkDone h

workFail :: Job -> IO ()
workFail (Job { handle = h, bc = c }) = withAgent c $ \agent ->
  send agent WorkFail h

schedLater :: Job -> Int64 -> IO ()
schedLater (Job { handle = h, bc =c }) later = withAgent c $ \agent ->
  send agent SchedLater $ B.concat [ h, nullChar, B.pack $ show later ]

schedLater' :: Job -> Int64 -> Int64 -> IO ()
schedLater' (Job { handle = h, bc = c }) later step = withAgent c $ \agent ->
  send agent SchedLater $ B.concat [ h
                                   , nullChar
                                   , B.pack $ show later
                                   , nullChar
                                   , B.pack $ show step
                                   ]
