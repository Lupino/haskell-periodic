{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server.Worker
  (
    Worker
  , Connection
  , newWorker
  , close
  , isAlive
  , startWorker
  , runWorker
  , getLastVist
  , workerId
  ) where

import           Control.Exception            (throwIO)
import           Control.Monad                (unless, when)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B (empty)
import           Data.Int                     (Int64)
import qualified Periodic.Connection          as Conn (Connection, connid)
import qualified Periodic.Lock                as L (Lock, new, with)

import           Periodic.Agent               (Agent, receive, send)
import           Periodic.IOList              (IOList, delete, elem, insert,
                                               newIOList, toList)
import           Periodic.Server.Scheduler
import           Prelude                      hiding (elem)

import           Periodic.Monad
import           Periodic.Types.Error         (Error (EmptyError))
import           Periodic.Types.Job           (FuncName, JobHandle)
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand
import           Periodic.Utils               (breakBS, getEpochTime, readBS)

import           Data.IORef                   (IORef, atomicModifyIORef',
                                               newIORef)

data WorkerEnv = WorkerEnv
  { wSched    :: Scheduler
  , wFuncList :: IOList FuncName
  , wJobQueue :: IOList JobHandle
  , wLastVist :: IORef Int64
  }

type Worker = GenPeriodic WorkerEnv
type Connection = SpecEnv WorkerEnv


newWorker :: Conn.Connection -> Scheduler -> IO Connection
newWorker conn wSched = do
  wFuncList <- newIOList
  wJobQueue <- newIOList
  wLastVist <- newIORef =<< getEpochTime

  let wEnv = WorkerEnv {..}

  env0 <- initEnv_ (handleAgent wEnv) conn wEnv
  runPeriodic env0 specEnv

runWorker :: Connection -> Worker a -> IO a
runWorker = runPeriodicWithSpecEnv

startWorker :: Connection -> IO () -> IO ()
startWorker env io = runPeriodicWithSpecEnv env $ do
  WorkerEnv {..} <- userEnv
  startMainLoop $ do
    mapM_ (failJob wSched) =<< toList wJobQueue
    mapM_ (removeFunc wSched) =<< toList wFuncList
    io

close :: Worker ()
close = stopPeriodic

getLastVist :: Worker Int64
getLastVist = do
  WorkerEnv {..} <- userEnv
  unsafeLiftIO $ atomicModifyIORef' wLastVist (\t -> (t, t))

workerId :: Worker ByteString
workerId = Conn.connid . conn <$> env

handleAgent :: WorkerEnv -> Agent -> IO ()
handleAgent w agent = do
  t <- getEpochTime
  atomicModifyIORef' (wLastVist w) (const (t, ()))
  cmd <- receive agent :: IO (Either String WorkerCommand)
  case cmd of
    Left e     ->throwIO EmptyError -- close worker
    Right cmd' -> go agent cmd'
  where go :: Agent -> WorkerCommand -> IO ()
        go ag GrabJob            = pushGrab sc fl jq ag
        go _ (WorkDone jh)       = doneJob sc jh >> delete jq jh
        go _ (WorkFail jh)       = failJob sc jh >> delete jq jh
        go _ (SchedLater jh l s) = schedLaterJob sc jh l s >> delete jq jh
        go ag Sleep              = send ag Noop
        go ag Ping               = send ag Pong
        go _ (CanDo fn)          = handleCanDo sc fl fn
        go _ (CantDo fn)         = handleCantDo sc fl fn
        go _ (Broadcast fn)      = handleBroadcast sc fl fn

        sc = wSched w
        fl = wFuncList w
        jq = wJobQueue w

handleCanDo :: Scheduler -> IOList FuncName -> FuncName -> IO ()
handleCanDo sched fl fn = do
  has <- elem fl fn
  unless has $ do
    addFunc sched fn
    insert fl fn

handleCantDo :: Scheduler -> IOList FuncName -> FuncName -> IO ()
handleCantDo sched fl fn = do
  has <- elem fl fn
  when has $ do
    removeFunc sched fn
    delete fl fn

handleBroadcast :: Scheduler -> IOList FuncName -> FuncName -> IO ()
handleBroadcast sched fl fn = do
  has <- elem fl fn
  unless has $ do
    broadcastFunc sched fn True
    insert fl fn
