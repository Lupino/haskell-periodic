module Periodic.JobM
  (
    JobM
  , runJob
  , workDone
  , workFail
  , schedLater
  , schedLater'

  , name
  , func
  , workload
  , counter
  ) where

import           Data.Int       (Int64)
import qualified Periodic.Job   as J
import           Periodic.Monad (GenPeriodic, env, runPeriodic, unsafeLiftIO')

type JobM = GenPeriodic J.Job ()

runJob :: J.Job -> JobM -> IO ()
runJob = runPeriodic

workDone :: JobM
workDone = unsafeLiftIO' J.workDone

workFail :: JobM
workFail = unsafeLiftIO' J.workFail

schedLater :: Int64 -> JobM
schedLater later = unsafeLiftIO' $ flip J.schedLater later

schedLater' :: Int64 -> Int64 -> JobM
schedLater' later step = unsafeLiftIO' $ \j -> J.schedLater' j later step

name = J.name <$> env
func = J.func <$> env
workload = J.workload <$> env
counter = J.counter <$> env
