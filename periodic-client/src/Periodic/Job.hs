module Periodic.Job
  (
    Job
  , JobEnv
  , initJobEnv
  , name
  , func
  , workload
  , counter

  , workDone
  , workFail
  , schedLater
  , schedLater'
  ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B (concat, pack)
import           Data.Int              (Int64)
import           Periodic.Agent        (send)
import           Periodic.Monad        (Env, GenPeriodic, cloneEnv, userEnv,
                                        withAgent)
import           Periodic.Types        (Command (..), nullChar)
import           Periodic.Types.Job    (FuncName, JobHandle, JobName, Workload)
import qualified Periodic.Types.Job    as J
import           Periodic.Utils        (breakBS)

data JobEnv = JobEnv { job :: J.Job, handle :: JobHandle }

type Job = GenPeriodic JobEnv

name :: Job JobName
name = J.jName . job <$> userEnv

func :: Job FuncName
func = J.jFuncName . job <$> userEnv

workload :: Job Workload
workload = J.jWorkload . job <$> userEnv

counter :: Job Int
counter = J.jCount . job <$> userEnv

initJobEnv :: ByteString -> Maybe JobEnv
initJobEnv = go . breakBS 2
  where go :: [ByteString] -> Maybe JobEnv
        go []        = Nothing
        go (_:[])    = Nothing
        go (h:dat:_) = parse h (J.decodeJob dat)

        parse :: ByteString -> Maybe J.Job -> Maybe JobEnv
        parse _ Nothing  = Nothing
        parse h (Just r) = Just $ JobEnv r h

workDone :: Job ()
workDone = do
  h <- handle <$> userEnv
  withAgent $ \agent -> send agent WorkDone h

workFail :: Job ()
workFail = do
  h <- handle <$> userEnv
  withAgent $ \agent -> send agent WorkFail h

schedLater :: Int64 -> Job ()
schedLater later = do
  h <- handle <$> userEnv
  withAgent $ \agent ->
    send agent SchedLater $ B.concat [ h, nullChar, B.pack $ show later ]

schedLater' :: Int64 -> Int64 -> Job ()
schedLater' later step = do
  h <- handle <$> userEnv
  withAgent $ \agent ->
    send agent SchedLater $ B.concat [ h
                                     , nullChar
                                     , B.pack $ show later
                                     , nullChar
                                     , B.pack $ show step
                                     ]
