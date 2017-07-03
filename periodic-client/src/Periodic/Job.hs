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
import qualified Data.ByteString.Char8 as B (concat, pack)
import           Periodic.Agent        (send)
import           Periodic.BaseClient   (BaseClient, withAgent)
import           Periodic.Types        (Command (..), nullChar)
import qualified Periodic.Types.Job    as J
import           Periodic.Utils        (breakBS)

import           Data.Int              (Int64)

data Job = Job { name     :: J.JobName
               , func     :: J.FuncName
               , workload :: J.Workload
               , counter  :: Int
               , bc       :: BaseClient
               , handle   :: J.JobHandle
               }

newJob :: BaseClient -> ByteString -> Maybe Job
newJob c = go . breakBS 2
  where go :: [ByteString] -> Maybe Job
        go []        = Nothing
        go (_:[])    = Nothing
        go (h:dat:_) = parse h (J.decodeJob dat)

        parse :: ByteString -> Maybe J.Job -> Maybe Job
        parse _ Nothing = Nothing
        parse h (Just r) = Just Job { name = J.jName r
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
