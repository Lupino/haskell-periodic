{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import           Periodic.Agent               (AgentT, agentEnv, liftAgentT,
                                               receive, send)
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
  { wFuncList :: IOList FuncName
  , wJobQueue :: IOList JobHandle
  , wLastVist :: TVar Int64
  }

type WorkerT m = PeriodicT (SchedT m) WorkerConfig

data WorkerEnv m = WorkerEnv
  { periodicEnv      :: Env (SchedT m) WorkerConfig
  , periodicState    :: PeriodicState
  , schedState       :: SchedState m
  , schedConfig      :: SchedConfig
  , connectionConfig :: Conn.ConnectionConfig
  , connectionState  :: Conn.ConnectionState
  }

runWorkerT :: Monad m => WorkerEnv m -> WorkerT m a -> m a
runWorkerT WorkerEnv {..} =
  runSchedT schedState schedConfig
    . Conn.runConnectionT connectionState connectionConfig
    . runPeriodicT periodicState periodicEnv

initWorkerEnv
  :: (MonadIO m, MonadBaseControl IO m)
  => Conn.ConnectionState
  -> Conn.ConnectionConfig
  -> SchedState m
  -> SchedConfig
  -> m (WorkerEnv m)
initWorkerEnv connectionState connectionConfig schedState schedConfig = do
  wFuncList <- liftIO newIOList
  wJobQueue <- liftIO newIOList
  wLastVist <- liftIO $ newTVarIO =<< getEpochTime

  let workerConfig = WorkerConfig {..}

  let periodicEnv = initEnv_ workerConfig $ handleAgentT workerConfig
  periodicState <- liftIO initPeriodicState
  return WorkerEnv{..}

startWorkerT
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => WorkerEnv m -> m ()
startWorkerT env0 = runWorkerT env0 $ do
  startMainLoop
  WorkerConfig {..} <- env
  mapM_ (liftPeriodicT . failJob) =<< liftIO (toList wJobQueue)
  mapM_ (liftPeriodicT . removeFunc) =<< liftIO (toList wFuncList)

close :: MonadIO m => WorkerT m ()
close = stopPeriodicT

getLastVist :: MonadIO m => WorkerT m Int64
getLastVist = do
  WorkerConfig {..} <- env
  liftIO $ readTVarIO wLastVist

handleAgentT :: (MonadIO m, MonadBaseControl IO m) => WorkerConfig -> AgentT (SchedT m) ()
handleAgentT WorkerConfig {..} = do
  liftIO $ do
    t <- getEpochTime
    atomically $ writeTVar wLastVist t
  cmd <- receive
  case cmd of
    Left _     -> lift . lift $ Conn.close -- close worker
    Right GrabJob -> do
      env0 <- agentEnv
      liftAgentT $ pushGrab wFuncList wJobQueue env0
    Right (WorkDone jh) -> do
      liftAgentT $ doneJob jh
      liftIO $ delete wJobQueue jh
    Right (WorkFail jh) -> do
      liftAgentT $ failJob jh
      liftIO $ delete wJobQueue jh
    Right (SchedLater jh l s) -> do
      liftAgentT $ schedLaterJob jh l s
      liftIO $ delete wJobQueue jh
    Right Sleep -> send Noop
    Right Ping -> send Pong
    Right (CanDo fn) -> do
      has <- liftIO $ elem wFuncList fn
      unless has $ do
        liftAgentT $ addFunc fn
        liftIO $ insert wFuncList fn
    Right (CantDo fn) -> do
      has <- liftIO $ elem wFuncList fn
      when has $ do
        liftAgentT $ removeFunc fn
        liftIO $ delete wFuncList fn
    Right (Broadcast fn) -> do
      has <- liftIO $ elem wFuncList fn
      unless has $ do
        liftAgentT $ broadcastFunc fn True
        liftIO $ insert wFuncList fn
