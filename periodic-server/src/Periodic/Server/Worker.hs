{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Periodic.Server.Worker
  (
    WorkerT
  , WorkerEnv
  , initWorkerEnv
  , close
  , startWorkerT
  , runWorkerT
  , getLastVist
  ) where

import           Control.Monad                (unless, when)
import           Data.Int                     (Int64)
import qualified Periodic.Connection          as Conn

import           Periodic.Agent               (AgentT, agent, receive, send)
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
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.STM            (atomically)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Control  (MonadBaseControl)

data WorkerConfig = WorkerConfig
  { wSched    :: Scheduler
  , wFuncList :: IOList FuncName
  , wJobQueue :: IOList JobHandle
  , wLastVist :: TVar Int64
  }

type WorkerT m = PeriodicT m WorkerConfig

data WorkerEnv m = WorkerEnv
  { periodicEnv   :: Env m WorkerConfig
  , periodicState :: PeriodicState
  }

runWorkerT :: Monad m => WorkerEnv m -> WorkerT m a -> m a
runWorkerT WorkerEnv {..} = runPeriodicT periodicState periodicEnv

initWorkerEnv
  :: (MonadIO m)
  => Conn.ConnectionState
  -> Conn.ConnectionConfig
  -> Scheduler
  -> m (WorkerEnv m)
initWorkerEnv connectionState connectionConfig wSched = do
  wFuncList <- liftIO $ newIOList
  wJobQueue <- liftIO $ newIOList
  wLastVist <- liftIO $ newTVarIO =<< getEpochTime

  let workerConfig = WorkerConfig {..}

  let env0 = initEnv_ workerConfig connectionConfig $ handleAgentT workerConfig
  state0 <- liftIO $ initPeriodicState connectionState
  return $ WorkerEnv env0 state0

startWorkerT
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => WorkerEnv m -> m ()
startWorkerT env0 = runWorkerT env0 $ do
  startMainLoop
  WorkerConfig {..} <- env
  liftIO $ mapM_ (failJob wSched) =<< toList wJobQueue
  liftIO $ mapM_ (removeFunc wSched) =<< toList wFuncList

close :: MonadIO m => WorkerT m ()
close = stopPeriodicT

getLastVist :: MonadIO m => WorkerT m Int64
getLastVist = do
  WorkerConfig {..} <- env
  liftIO $ readTVarIO wLastVist

handleAgentT :: MonadIO m => WorkerConfig -> AgentT m ()
handleAgentT WorkerConfig {..} = do
  liftIO $ do
    t <- getEpochTime
    atomically $ writeTVar wLastVist t
  cmd <- receive
  case cmd of
    Left _     -> lift . lift $ Conn.close -- close worker
    Right GrabJob -> do
      ag <- agent
      liftIO $ pushGrab wSched wFuncList wJobQueue ag
    Right (WorkDone jh) -> liftIO $ do
      doneJob wSched jh
      delete wJobQueue jh
    Right (WorkFail jh) -> liftIO $ do
      failJob wSched jh
      delete wJobQueue jh
    Right (SchedLater jh l s) -> liftIO $ do
      schedLaterJob wSched jh l s
      delete wJobQueue jh
    Right Sleep -> send Noop
    Right Ping -> send Pong
    Right (CanDo fn) -> liftIO $ do
      has <- elem wFuncList fn
      unless has $ do
        addFunc wSched fn
        insert wFuncList fn
    Right (CantDo fn) -> liftIO $ do
      has <- elem wFuncList fn
      when has $ do
        removeFunc wSched fn
        delete wFuncList fn
    Right (Broadcast fn) -> liftIO $ do
      has <- elem wFuncList fn
      unless has $ do
        broadcastFunc wSched fn True
        insert wFuncList fn
