module Periodic.Job
  (
    JobT
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

  , acquireLock
  , releaseLock

  , withLock
  ) where

import           Control.Monad                (when)
import           Control.Monad.Catch          (MonadMask)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.ByteString              (ByteString, empty)
import           Data.Int                     (Int64)
import           Periodic.Agent               (receive, send)
import           Periodic.Node
import           Periodic.Types               (FromBS (..), LockName)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand (ServerCommand (Acquired))
import           Periodic.Types.WorkerCommand

type JobT m = NodeT Job m

name :: (FromBS a, Show a, Monad m) => JobT m a
name = fromBS . unJN <$> name_

name_ :: Monad m => JobT m JobName
name_ = getName <$> env

func :: (FromBS a, Show a, Monad m) => JobT m a
func = fromBS . unFN <$> func_

func_ :: Monad m => JobT m FuncName
func_ = getFuncName <$> env

workload :: (FromBS a, Show a, Monad m) => JobT m a
workload = fromBS . unWL <$> workload_

workload_ :: Monad m => JobT m Workload
workload_ = getWorkload <$> env

count :: Monad m => JobT m Int
count = getCount <$> env

timeout :: Monad m => JobT m Int
timeout = getTimeout <$> env

workDone
  :: (MonadIO m, MonadMask m)
  => JobT m ()
workDone = workDone_ empty

workDone_
  :: (MonadIO m, MonadMask m)
  => ByteString -> JobT m ()
workDone_ w = do
  h <- getHandle <$> env
  withAgentT $ send (WorkDone h w)

workFail
  :: (MonadIO m, MonadMask m)
  =>  JobT m ()
workFail = do
  h <- getHandle <$> env
  withAgentT $ send (WorkFail h)

schedLater
  :: (MonadIO m, MonadMask m)
  =>  Int64 -> JobT m ()
schedLater later = do
  h <- getHandle <$> env
  withAgentT $ send (SchedLater h later 0)

schedLater'
  :: (MonadIO m, MonadMask m)
  =>  Int64 -> Int -> JobT m ()
schedLater' later step = do
  h <- getHandle <$> env
  withAgentT $ send (SchedLater h later step)

acquireLock
  :: (MonadIO m, MonadMask m)
  => LockName -> Int -> JobT m Bool
acquireLock n maxCount = do
  h <- getHandle <$> env
  withAgentT $ do
    send (Acquire n maxCount h)
    r <- receive
    case r of
      Right (Acquired v) -> pure v
      _                  -> pure False

releaseLock
  :: (MonadIO m, MonadMask m)
  => LockName -> JobT m ()
releaseLock n = do
  h <- getHandle <$> env
  withAgentT $ send (Release n h)

withLock
  :: (MonadIO m, MonadMask m)
  => LockName -> Int -> JobT m () -> JobT m ()
withLock n maxCount j = do
  acquired <- acquireLock n maxCount
  when acquired $ do
    j
    releaseLock n
