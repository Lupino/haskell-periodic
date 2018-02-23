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

import           Control.Monad                (unless, when)
import           Data.ByteString              (ByteString)
import           Data.Int                     (Int64)
import qualified Periodic.Connection          as Conn (Connection, connid)

import           Periodic.Agent               (Agent, receive, send)
import           Periodic.IOList              (IOList, delete, elem, insert,
                                               newIOList, toList)
import           Periodic.Server.Scheduler
import           Prelude                      hiding (elem)

import           Periodic.Monad
import           Periodic.Types.Job           (FuncName, JobHandle)
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand
import           Periodic.Utils               (getEpochTime)

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM            (atomically)

data WorkerEnv = WorkerEnv
  { wSched    :: Scheduler
  , wFuncList :: IOList FuncName
  , wJobQueue :: IOList JobHandle
  , wLastVist :: TVar Int64
  }

type Worker = GenPeriodic WorkerEnv
type Connection = SpecEnv WorkerEnv


newWorker :: Conn.Connection -> Scheduler -> IO Connection
newWorker conn0 wSched = do
  wFuncList <- newIOList
  wJobQueue <- newIOList
  wLastVist <- newTVarIO =<< getEpochTime

  let wEnv = WorkerEnv {..}

  env0 <- initEnv_ handleAgent conn0 wEnv
  runPeriodic env0 specEnv

runWorker :: Connection -> Worker a -> IO a
runWorker = runPeriodicWithSpecEnv

startWorker :: Connection -> IO () -> IO ()
startWorker env0 io = runPeriodicWithSpecEnv env0 $ do
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
  unsafeLiftIO $ readTVarIO wLastVist

workerId :: Worker ByteString
workerId = Conn.connid . conn <$> env

handleAgent :: Agent -> Worker ()
handleAgent agent = do
  WorkerEnv {..} <- userEnv
  unsafeLiftIO $ do
    t <- getEpochTime
    atomically $ writeTVar wLastVist t
  cmd <- unsafeLiftIO $ receive agent :: Worker (Either String WorkerCommand)
  case cmd of
    Left _     -> close -- close worker
    Right GrabJob -> unsafeLiftIO $ pushGrab wSched wFuncList wJobQueue agent
    Right (WorkDone jh) -> unsafeLiftIO $ do
      doneJob wSched jh
      delete wJobQueue jh
    Right (WorkFail jh) -> unsafeLiftIO $ do
      failJob wSched jh
      delete wJobQueue jh
    Right (SchedLater jh l s) -> unsafeLiftIO $ do
      schedLaterJob wSched jh l s
      delete wJobQueue jh
    Right Sleep -> unsafeLiftIO $ send agent Noop
    Right Ping -> unsafeLiftIO $ send agent Pong
    Right (CanDo fn) -> unsafeLiftIO $ do
      has <- elem wFuncList fn
      unless has $ do
        addFunc wSched fn
        insert wFuncList fn
    Right (CantDo fn) -> unsafeLiftIO $ do
      has <- elem wFuncList fn
      when has $ do
        removeFunc wSched fn
        delete wFuncList fn
    Right (Broadcast fn) -> unsafeLiftIO $ do
      has <- elem wFuncList fn
      unless has $ do
        broadcastFunc wSched fn True
        insert wFuncList fn
