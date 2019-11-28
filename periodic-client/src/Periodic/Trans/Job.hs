module Periodic.Trans.Job
  ( JobT
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
  , withLock_
  ) where

import           Control.Monad                (when)
import           Data.ByteString              (ByteString, empty)
import           Data.Int                     (Int64)
import           Periodic.Agent               (receive, send)
import           Periodic.Node
import           Periodic.Transport           (Transport)
import           Periodic.Types               (FromBS (..), LockName)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand (ServerCommand (Acquired))
import           Periodic.Types.WorkerCommand
import           UnliftIO                     hiding (timeout)

type JobT tp m = NodeT Job tp m

name :: (FromBS a, Show a, Monad m) => JobT tp m a
name = fromBS . unJN <$> name_

name_ :: Monad m => JobT tp m JobName
name_ = getName <$> env

func :: (FromBS a, Show a, Monad m) => JobT tp m a
func = fromBS . unFN <$> func_

func_ :: Monad m => JobT tp m FuncName
func_ = getFuncName <$> env

workload :: (FromBS a, Show a, Monad m) => JobT tp m a
workload = fromBS . unWL <$> workload_

workload_ :: Monad m => JobT tp m Workload
workload_ = getWorkload <$> env

count :: Monad m => JobT tp m Int
count = getCount <$> env

timeout :: Monad m => JobT tp m Int
timeout = getTimeout <$> env

workDone
  :: (MonadUnliftIO m, Transport tp)
  => JobT tp m ()
workDone = workDone_ empty

workDone_
  :: (MonadUnliftIO m, Transport tp)
  => ByteString -> JobT tp m ()
workDone_ w = do
  h <- getHandle <$> env
  withAgentT $ send (WorkDone h w)

workFail
  :: (MonadUnliftIO m, Transport tp)
  =>  JobT tp m ()
workFail = do
  h <- getHandle <$> env
  withAgentT $ send (WorkFail h)

schedLater
  :: (MonadUnliftIO m, Transport tp)
  =>  Int64 -> JobT tp m ()
schedLater later = do
  h <- getHandle <$> env
  withAgentT $ send (SchedLater h later 0)

schedLater'
  :: (MonadUnliftIO m, Transport tp)
  =>  Int64 -> Int -> JobT tp m ()
schedLater' later step = do
  h <- getHandle <$> env
  withAgentT $ send (SchedLater h later step)

acquireLock
  :: (MonadUnliftIO m, Transport tp)
  => LockName -> Int -> JobT tp m Bool
acquireLock n maxCount = do
  h <- getHandle <$> env
  withAgentT $ do
    send (Acquire n maxCount h)
    r <- receive
    case r of
      Right (Acquired v) -> pure v
      _                  -> pure False

releaseLock
  :: (MonadUnliftIO m, Transport tp)
  => LockName -> JobT tp m ()
releaseLock n = do
  h <- getHandle <$> env
  withAgentT $ send (Release n h)

withLock_
  :: (MonadUnliftIO m, Transport tp)
  => LockName -> Int -> JobT tp m () -> JobT tp m ()
withLock_ n maxCount j = do
  acquired <- acquireLock n maxCount
  when acquired j

withLock
  :: (MonadUnliftIO m, Transport tp)
  => LockName -> Int -> JobT tp m () -> JobT tp m ()
withLock n maxCount j = do
  withLock_ n maxCount j
  releaseLock n
