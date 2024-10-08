module Periodic.Trans.Job
  ( JobT
  , JobEnv
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
  , workData
  , workFail
  , schedLater
  , schedLater'

  , acquireLock
  , releaseLock

  , withLock
  , withLock_
  , module Periodic.Trans.BaseClient
  ) where

import           Control.Monad                (void, when)
import           Data.ByteString              (ByteString, empty)
import           Data.Int                     (Int64)
import           Data.Maybe                   (fromJust)
import           Metro.Class                  (Transport)
import           Metro.Node                   (env, request)
import           Periodic.Node
import           Periodic.Trans.BaseClient    (BaseClientT, runJob, runJob_,
                                               submitJob, submitJob_,
                                               successRequest)
import           Periodic.Types               (FromBS (..), LockName, getResult,
                                               packetREQ)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand (ServerCommand (Acquired))
import           Periodic.Types.WorkerCommand
import           UnliftIO                     hiding (timeout)

type JobT = BaseClientT (Maybe Job)
type JobEnv = SessionEnv (Maybe Job) ServerCommand

job :: Monad m => JobT tp m Job
job = fromJust <$> env

name :: (FromBS a, Show a, Monad m) => JobT tp m a
name = fromBS . unJN <$> name_

name_ :: Monad m => JobT tp m JobName
name_ = getName <$> job

func :: (FromBS a, Show a, Monad m) => JobT tp m a
func = fromBS . unFN <$> func_

func_ :: Monad m => JobT tp m FuncName
func_ = getFuncName <$> job

workload :: (FromBS a, Show a, Monad m) => JobT tp m a
workload = fromBS . unWL <$> workload_

workload_ :: Monad m => JobT tp m Workload
workload_ = getWorkload <$> job

count :: Monad m => JobT tp m Int
count = getCount <$> job

timeout :: Monad m => JobT tp m Int
timeout = getTimeout <$> job

workDone
  :: (MonadUnliftIO m, Transport tp)
  => JobT tp m Bool
workDone = workDone_ empty

workDone_
  :: (MonadUnliftIO m, Transport tp)
  => ByteString -> JobT tp m Bool
workDone_ w = do
  h <- getHandle <$> job
  successRequest (packetREQ $ WorkDone h w)

workData
  :: (MonadUnliftIO m, Transport tp)
  => ByteString -> JobT tp m Bool
workData w = do
  h <- getHandle <$> job
  successRequest (packetREQ $ WorkData h w)

workFail
  :: (MonadUnliftIO m, Transport tp)
  =>  JobT tp m Bool
workFail = do
  h <- getHandle <$> job
  successRequest (packetREQ $ WorkFail h)

schedLater
  :: (MonadUnliftIO m, Transport tp)
  =>  Int64 -> JobT tp m Bool
schedLater later = do
  h <- getHandle <$> job
  successRequest (packetREQ $ SchedLater h later 0)

schedLater'
  :: (MonadUnliftIO m, Transport tp)
  =>  Int64 -> Int -> JobT tp m Bool
schedLater' later step = do
  h <- getHandle <$> job
  successRequest (packetREQ $ SchedLater h later step)

acquireLock
  :: (MonadUnliftIO m, Transport tp)
  => LockName -> Int -> JobT tp m Bool
acquireLock n maxCount = do
  h <- getHandle <$> job
  getResult False getAcq <$> request Nothing (packetREQ (Acquire n maxCount h))

  where getAcq :: ServerCommand -> Bool
        getAcq (Acquired v) = v
        getAcq _            = False

releaseLock
  :: (MonadUnliftIO m, Transport tp)
  => LockName -> JobT tp m Bool
releaseLock n = do
  h <- getHandle <$> job
  successRequest (packetREQ $ Release n h)

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
  void $ releaseLock n
