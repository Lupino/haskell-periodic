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
import           Periodic.Agent               (AgentT, agentEnv', receive, send)
import           Periodic.Connection          (ConnEnv, fromConn,
                                               runConnectionT)
import qualified Periodic.Connection          as Conn
import           Periodic.IOList              (IOList, delete, elem, insert,
                                               newIOList, toList)
import           Periodic.Node
import           Periodic.Server.Persist      (Persist)
import           Periodic.Server.Scheduler
import           Periodic.Transport           (Transport)
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

type WorkerT db tp m = NodeT WorkerConfig tp (SchedT db tp m)

data WorkerEnv db tp = WorkerEnv
  { nodeEnv  :: NodeEnv WorkerConfig
  , schedEnv :: SchedEnv db tp
  , connEnv  :: ConnEnv tp
  }

runWorkerT :: Monad m => WorkerEnv db tp -> WorkerT db tp m a -> m a
runWorkerT WorkerEnv {..} =
  runSchedT schedEnv
    . runConnectionT connEnv
    . runNodeT nodeEnv

initWorkerEnv
  :: (MonadIO m)
  => ConnEnv tp
  -> SchedEnv db tp
  -> m (WorkerEnv db tp)
initWorkerEnv connEnv schedEnv = do
  wFuncList <- newIOList
  wJobQueue <- newIOList
  wLastVist <- newTVarIO =<< getEpochTime

  let workerConfig = WorkerConfig {..}

  nodeEnv <- initEnv workerConfig
  return WorkerEnv{..}

startWorkerT :: (MonadUnliftIO m, Persist db, Transport tp) => WorkerEnv db tp -> m ()
startWorkerT env0 = runWorkerT env0 $ do
  startMainLoop_ . handleAgentT =<< env

  WorkerConfig {..} <- env

  mapM_ (lift . failJob) =<< toList wJobQueue
  mapM_ (lift . removeFunc) =<< toList wFuncList

close :: (MonadIO m, Transport tp) => WorkerT db tp m ()
close = stopNodeT

getLastVist :: MonadIO m => WorkerT db tp m Int64
getLastVist = do
  WorkerConfig {..} <- env
  readTVarIO wLastVist

handleAgentT :: (MonadUnliftIO m, Persist db, Transport tp) => WorkerConfig -> AgentT tp (SchedT db tp m) ()
handleAgentT WorkerConfig {..} = do
  t <- getEpochTime
  atomically $ writeTVar wLastVist t
  cmd <- receive
  case cmd of
    Left e -> do
      liftIO $ errorM "Periodic.Server.Worker" $ "Worker error: " ++ e
      fromConn Conn.close -- close worker
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
