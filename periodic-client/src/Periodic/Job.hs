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

import           Data.Byteable                (toBytes)
import           Data.Int                     (Int64)
import           Periodic.Agent               (send)
import           Periodic.Monad               (GenPeriodic, userEnv, withAgent)
import           Periodic.Types               (FromBS (..), JobHandle)
import qualified Periodic.Types.Job           as J
import           Periodic.Types.WorkerCommand

data JobEnv = JobEnv { job :: J.Job, handle :: JobHandle }

type Job = GenPeriodic JobEnv

name :: (FromBS a, Show a) => Job a
name = fromBS . toBytes . J.jName . job <$> userEnv

func :: (FromBS a, Show a) => Job a
func = fromBS . toBytes . J.jFuncName . job <$> userEnv

workload :: (FromBS a, Show a) => Job a
workload = fromBS . toBytes . J.jWorkload . job <$> userEnv

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
