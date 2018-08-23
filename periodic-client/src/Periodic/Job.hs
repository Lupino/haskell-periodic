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
  , count
  , timeout

  , workDone
  , workDone_
  , workFail
  , schedLater
  , schedLater'
  ) where

import           Control.Monad.Catch          (MonadMask)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.ByteString              (ByteString, empty)
import           Data.Int                     (Int64)
import           Periodic.Agent               (send)
import           Periodic.Node
import           Periodic.Types               (FromBS (..))
import           Periodic.Types.Job
import           Periodic.Types.WorkerCommand

data JobConfig = JobConfig { job :: Job, handle :: JobHandle }

type JobT m = NodeT JobConfig m

name :: (FromBS a, Show a, Monad m) => JobT m a
name = fromBS . unJN <$> name_

name_ :: Monad m => JobT m JobName
name_ = getName . job <$> env

func :: (FromBS a, Show a, Monad m) => JobT m a
func = fromBS . unFN <$> func_

func_ :: Monad m => JobT m FuncName
func_ = getFuncName . job <$> env

workload :: (FromBS a, Show a, Monad m) => JobT m a
workload = fromBS . unWL <$> workload_

workload_ :: Monad m => JobT m Workload
workload_ = getWorkload . job <$> env

count :: Monad m => JobT m Int
count = getCount . job <$> env

timeout :: Monad m => JobT m Int
timeout = getTimeout . job <$> env

initJobConfig :: Job -> JobHandle -> JobConfig
initJobConfig = JobConfig

workDone
  :: (MonadIO m, MonadMask m)
  => JobT m ()
workDone = workDone_ empty

workDone_
  :: (MonadIO m, MonadMask m)
  => ByteString -> JobT m ()
workDone_ w = do
  h <- handle <$> env
  withAgentT $ send (WorkDone h w)

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
