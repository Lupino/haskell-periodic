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
import           Periodic.Agent               (AgentT, agentEnv', liftC,
                                               receive, send)
import           Periodic.Connection          (ConnEnv, runConnectionT)
import qualified Periodic.Connection          as Conn
import           Periodic.IOList              (IOList, delete, elem, insert,
                                               newIOList, toList)
import           Periodic.Server.Scheduler
import           Prelude                      hiding (elem)

import           Control.Concurrent.STM.TVar
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Haskey         (MonadHaskey)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.STM            (atomically)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Periodic.Node                hiding (liftC)
import           Periodic.Types.Job           (FuncName, JobHandle)
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand
import           Periodic.Utils               (getEpochTime)

data WorkerConfig = WorkerConfig
  { wFuncList :: IOList FuncName
  , wJobQueue :: IOList JobHandle
  , wLastVist :: TVar Int64
  }

type WorkerT m = NodeT WorkerConfig (SchedT m)

data WorkerEnv = WorkerEnv
  { nodeEnv  :: NodeEnv WorkerConfig
  , schedEnv :: SchedEnv
  , connEnv  :: ConnEnv
  }

runWorkerT :: Monad m => WorkerEnv -> WorkerT m a -> m a
runWorkerT WorkerEnv {..} =
  runSchedT schedEnv
    . runConnectionT connEnv
    . runNodeT nodeEnv

initWorkerEnv
  :: (MonadIO m)
  => ConnEnv
  -> SchedEnv
  -> m WorkerEnv
initWorkerEnv connEnv schedEnv = do
  wFuncList <- liftIO newIOList
  wJobQueue <- liftIO newIOList
  wLastVist <- liftIO $ newTVarIO =<< getEpochTime

  let workerConfig = WorkerConfig {..}

  nodeEnv <- liftIO $ initEnv workerConfig
  return WorkerEnv{..}

startWorkerT
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m, MonadHaskey Schema m)
  => WorkerEnv -> m ()
startWorkerT env0 = runWorkerT env0 $ do
  startMainLoop_ . handleAgentT =<< env

  WorkerConfig {..} <- env

  mapM_ (lift . failJob) =<< liftIO (toList wJobQueue)
  mapM_ (lift . removeFunc) =<< liftIO (toList wFuncList)

close :: MonadIO m => WorkerT m ()
close = stopNodeT

getLastVist :: MonadIO m => WorkerT m Int64
getLastVist = do
  WorkerConfig {..} <- env
  liftIO $ readTVarIO wLastVist

handleAgentT :: (MonadIO m, MonadBaseControl IO m, MonadHaskey Schema m) => WorkerConfig -> AgentT (SchedT m) ()
handleAgentT WorkerConfig {..} = do
  liftIO $ do
    t <- getEpochTime
    atomically $ writeTVar wLastVist t
  cmd <- receive
  case cmd of
    Left _     -> liftC Conn.close -- close worker
    Right GrabJob -> do
      env0 <- agentEnv'
      lift $ pushGrab wFuncList wJobQueue env0
    Right (WorkDone jh) -> do
      lift $ doneJob jh
      liftIO $ delete wJobQueue jh
    Right (WorkFail jh) -> do
      lift $ failJob jh
      liftIO $ delete wJobQueue jh
    Right (SchedLater jh l s) -> do
      lift $ schedLaterJob jh l s
      liftIO $ delete wJobQueue jh
    Right Sleep -> send Noop
    Right Ping -> send Pong
    Right (CanDo fn) -> do
      has <- liftIO $ elem wFuncList fn
      unless has $ do
        lift $ addFunc fn
        liftIO $ insert wFuncList fn
    Right (CantDo fn) -> do
      has <- liftIO $ elem wFuncList fn
      when has $ do
        lift $ removeFunc fn
        liftIO $ delete wFuncList fn
    Right (Broadcast fn) -> do
      has <- liftIO $ elem wFuncList fn
      unless has $ do
        lift $ broadcastFunc fn True
        liftIO $ insert wFuncList fn
