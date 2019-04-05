{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Periodic.Server.Worker
  ( WorkerT
  , WorkerEnv
  , initWorkerEnv
  , close
  , startWorkerT
  , runWorkerT
  , getLastVist
  ) where

import           Control.Monad                (unless, when)
import           Control.Monad.Trans.Class    (lift)
import           Data.Int                     (Int64)
import           Periodic.Agent               (AgentT, agentEnv', liftC,
                                               receive, send)
import           Periodic.Connection          (ConnEnv, runConnectionT)
import qualified Periodic.Connection          as Conn
import           Periodic.IOList              (IOList, delete, elem, insert,
                                               newIOList, toList)
import           Periodic.Node                hiding (liftC)
import           Periodic.Server.Scheduler
import           Periodic.Types.Job           (FuncName, JobHandle)
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand
import           Periodic.Utils               (getEpochTime)
import           Prelude                      hiding (elem)
import           System.Log.Logger            (errorM)
import           UnliftIO

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
  wFuncList <- newIOList
  wJobQueue <- newIOList
  wLastVist <- newTVarIO =<< liftIO getEpochTime

  let workerConfig = WorkerConfig {..}

  nodeEnv <- liftIO $ initEnv workerConfig
  return WorkerEnv{..}

startWorkerT :: MonadUnliftIO m => WorkerEnv -> m ()
startWorkerT env0 = runWorkerT env0 $ do
  startMainLoop_ . handleAgentT =<< env

  WorkerConfig {..} <- env

  mapM_ (lift . failJob) =<< toList wJobQueue
  mapM_ (lift . removeFunc) =<< toList wFuncList

close :: MonadIO m => WorkerT m ()
close = stopNodeT

getLastVist :: MonadIO m => WorkerT m Int64
getLastVist = do
  WorkerConfig {..} <- env
  readTVarIO wLastVist

handleAgentT :: MonadUnliftIO m => WorkerConfig -> AgentT (SchedT m) ()
handleAgentT WorkerConfig {..} = do
  t <- liftIO getEpochTime
  atomically $ writeTVar wLastVist t
  cmd <- receive
  case cmd of
    Left e -> do
      liftIO $ errorM "Periodic.Server.Worker" $ "Worker error: " ++ e
      liftC Conn.close -- close worker
    Right GrabJob -> do
      env0 <- agentEnv'
      lift $ pushGrab wFuncList wJobQueue env0
    Right (WorkDone jh w) -> do
      lift $ doneJob jh w
      delete wJobQueue jh
    Right (WorkFail jh) -> do
      lift $ failJob jh
      delete wJobQueue jh
    Right (SchedLater jh l s) -> do
      lift $ schedLaterJob jh l s
      delete wJobQueue jh
    Right Sleep -> send Noop
    Right Ping -> send Pong
    Right (CanDo fn) -> do
      has <- elem wFuncList fn
      unless has $ do
        lift $ addFunc fn
        insert wFuncList fn
    Right (CantDo fn) -> do
      has <- elem wFuncList fn
      when has $ do
        lift $ removeFunc fn
        delete wFuncList fn
    Right (Broadcast fn) -> do
      has <- elem wFuncList fn
      unless has $ do
        lift $ broadcastFunc fn True
        insert wFuncList fn
    Right (Acquire n c jh) -> do
      r <- lift $ acquireLock n c jh
      send $ Acquired r
    Right (Release n jh) -> lift $ releaseLock n jh
