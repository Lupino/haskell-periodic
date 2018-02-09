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

import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B (concat, pack)
import           Data.Int                     (Int64)
import           Periodic.Agent               (send)
import           Periodic.Monad               (GenPeriodic, userEnv, withAgent)
import           Periodic.Types.Job           (FuncName, JobHandle, JobName,
                                               Workload)
import qualified Periodic.Types.Job           as J
import           Periodic.Types.WorkerCommand
import           Periodic.Utils               (breakBS)

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

initJobEnv :: J.Job -> JobHandle -> JobEnv
initJobEnv = JobEnv

workDone :: Job ()
workDone = do
  h <- handle <$> userEnv
  withAgent $ \agent -> send agent (WorkDone h)

workFail :: Job ()
workFail = do
  h <- handle <$> userEnv
  withAgent $ \agent -> send agent (WorkFail h)

schedLater :: Int64 -> Job ()
schedLater later = do
  h <- handle <$> userEnv
  withAgent $ \agent ->
    send agent (SchedLater h later 0)

schedLater' :: Int64 -> Int -> Job ()
schedLater' later step = do
  h <- handle <$> userEnv
  withAgent $ \agent ->
    send agent (SchedLater h later step)
