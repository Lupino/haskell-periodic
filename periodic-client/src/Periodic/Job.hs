module Periodic.Job
  (
    JobT
  , JobConfig
  , initJobConfig
  , name
  , func
  , workload
  , name_
  , func_
  , workload_
  , counter

  , workDone
  , workFail
  , schedLater
  , schedLater'
  ) where

import           Control.Monad.Catch          (MonadMask)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Int                     (Int64)
import           Periodic.Agent               (send)
import           Periodic.Monad
import           Periodic.Types               (FromBS (..), FuncName (..),
                                               JobHandle, JobName (..),
                                               Workload (..))
import qualified Periodic.Types.Job           as J
import           Periodic.Types.WorkerCommand

data JobConfig = JobConfig { job :: J.Job, handle :: JobHandle }

type JobT m = PeriodicT m JobConfig

name :: (FromBS a, Show a, Monad m) => JobT m a
name = fromBS . unJN <$> name_

name_ :: Monad m => JobT m JobName
name_ = J.jName . job <$> env

func :: (FromBS a, Show a, Monad m) => JobT m a
func = fromBS . unFN <$> func_

func_ :: Monad m => JobT m FuncName
func_ = J.jFuncName . job <$> env

workload :: (FromBS a, Show a, Monad m) => JobT m a
workload = fromBS . unWL <$> workload_

workload_ :: Monad m => JobT m Workload
workload_ = J.jWorkload . job <$> env

counter :: Monad m => JobT m Int
counter = J.jCount . job <$> env

initJobConfig :: J.Job -> JobHandle -> JobConfig
initJobConfig = JobConfig

workDone
  :: (MonadIO m, MonadMask m)
  =>  JobT m ()
workDone = do
  h <- handle <$> env
  withAgentT $ send (WorkDone h)

workFail
  :: (MonadIO m, MonadMask m)
  =>  JobT m ()
workFail = do
  h <- handle <$> env
  withAgentT $ send (WorkFail h)

schedLater
  :: (MonadIO m, MonadMask m)
  =>  Int64 -> JobT m ()
schedLater later = do
  h <- handle <$> env
  withAgentT $ send (SchedLater h later 0)

schedLater'
  :: (MonadIO m, MonadMask m)
  =>  Int64 -> Int -> JobT m ()
schedLater' later step = do
  h <- handle <$> env
  withAgentT $ send (SchedLater h later step)
