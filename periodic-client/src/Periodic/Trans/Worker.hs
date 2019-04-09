{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Periodic.Trans.Worker
  ( WorkerT
  , runWorkerT
  , ping
  , addFunc
  , broadcast
  , removeFunc
  , work
  , close
  ) where

import           Control.Monad                (forever, replicateM, unless,
                                               void, when)
import           Data.Byteable                (toBytes)
import           Data.Maybe                   (fromJust, isJust)
import           Periodic.Agent               (AgentEnv, readerSize, receive,
                                               runAgentT, send)
import           Periodic.Connection          (fromConn, initClientConnEnv)
import qualified Periodic.Connection          as Conn
import           Periodic.IOHashMap           (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap           as HM (delete, insert, lookup)
import           Periodic.Node
import           Periodic.Trans.Job           (JobT, func_, name, workFail)
import           Periodic.Transport           (Transport, TransportConfig)
import           Periodic.Types               (ClientType (TypeWorker),
                                               FuncName, Job)
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand
import           System.Log.Logger            (errorM)
import           UnliftIO
import           UnliftIO.Concurrent          (threadDelay)

type TaskList tp m = IOHashMap FuncName (JobT tp m ())
type WorkerT tp m = NodeT (TaskList tp m) tp m

runWorkerT
  :: (MonadUnliftIO m, Transport tp)
  => TransportConfig tp -> WorkerT tp m a -> m a
runWorkerT config m = do
  connEnv <- initClientConnEnv config
  Conn.runConnectionT connEnv $ do
    Conn.send $ toBytes TypeWorker
    void Conn.receive

    taskList <- newIOHashMap
    env0 <- initEnv taskList

    runNodeT env0 $ do
      void $ async $ forever $ do
        threadDelay $ 100 * 1000 * 1000
        checkHealth
      void $ async startMainLoop
      m

close :: (MonadUnliftIO m, Transport tp) => WorkerT tp m ()
close = stopNodeT

ping :: (MonadUnliftIO m, Transport tp) => WorkerT tp m Bool
ping = withAgentT $ do
  send Ping
  ret <- receive
  case ret of
    Left _     -> return False
    Right Pong -> return True
    Right _    -> return False

addFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobT tp m () -> WorkerT tp m ()
addFunc f j = do
  withAgentT $ send (CanDo f)
  ref <- env
  HM.insert ref f j

broadcast
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobT tp m () -> WorkerT tp m ()
broadcast f j = do
  withAgentT $ send (Broadcast f)
  ref <- env
  HM.insert ref f j

removeFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> WorkerT tp m ()
removeFunc f = do
  withAgentT $ send (CantDo f)
  ref <- env
  HM.delete ref f

grabJob
  :: (MonadUnliftIO m, Transport tp)
  => AgentEnv -> WorkerT tp m (Maybe Job)
grabJob agentEnv = do
  pl <- fromConn . runAgentT agentEnv $ do
    size <- readerSize
    when (size == 0) $ send GrabJob
    timeout 10000000 receive

  case pl of
    Nothing                      -> pure Nothing
    Just (Right (JobAssign job)) -> pure (Just job)
    _                            -> pure Nothing


work
  :: (MonadUnliftIO m, Transport tp)
  => Int -> WorkerT tp m ()
work size = do
  asyncs <- replicateM size $ async work_
  void $ waitAnyCancel asyncs

work_ :: (MonadUnliftIO m, Transport tp) => WorkerT tp m ()
work_ = do
  taskList <- env
  agentEnv <- newAgentEnv
  forever $ do
    j <- grabJob agentEnv
    when (isJust j) $
      withEnv (fromJust j) $ do
        f <- func_
        task <- HM.lookup taskList f
        case task of
          Nothing -> do
            withEnv taskList $ removeFunc f
            workFail
          Just task' ->
            catchAny task' $ \e -> do
              n <- name
              liftIO $ errorM "Periodic.Worker"
                     $ concat [ "Failing on running job { name = "
                              , n
                              , ", "
                              , show f
                              , " }"
                              , "\nError: "
                              , show e
                              ]
              workFail

checkHealth
  :: (MonadUnliftIO m, Transport tp)
  => WorkerT tp m ()
checkHealth = do
  ret <- timeout 10000000 ping
  case ret of
    Nothing -> close
    Just r  -> unless r close
