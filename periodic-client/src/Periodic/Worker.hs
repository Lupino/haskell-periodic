{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Periodic.Worker
  (
    WorkerT
  , runWorkerT
  , ping
  , addFunc
  , broadcast
  , removeFunc
  , work
  , close
  ) where

import           Control.Concurrent           (threadDelay)
import           Control.Exception            (SomeException)
import           Control.Monad                (forever, replicateM, unless,
                                               void, when)
import           Data.Byteable                (toBytes)
import           Data.Maybe                   (fromJust, isJust)
import           Periodic.Agent               (AgentEnv, readerSize, receive,
                                               runAgentT, send)
import           Periodic.Connection          (initClientConnEnv)
import qualified Periodic.Connection          as Conn
import           Periodic.IOHashMap           (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap           as HM (delete, insert, lookup)
import           Periodic.Job                 (JobT, func_, name, workFail)
import           Periodic.Node
import           Periodic.Socket              (connect)
import           Periodic.Transport           (Transport, makeSocketTransport)
import           Periodic.Types               (ClientType (TypeWorker),
                                               FuncName, Job)
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand
import           System.Log.Logger            (errorM)
import           UnliftIO

type TaskList m = IOHashMap FuncName (JobT m ())
type WorkerT m = NodeT (TaskList m) m

runWorkerT
  :: MonadUnliftIO m
  => (Transport -> IO Transport) -> String -> WorkerT m a -> m a
runWorkerT f h m = do
  connEnv <- liftIO $
    initClientConnEnv
      =<< f
      =<< makeSocketTransport
      =<< connect h
  Conn.runConnectionT connEnv $ do
    Conn.send $ toBytes TypeWorker
    void Conn.receive

    taskList <- newIOHashMap
    env0 <- liftIO $ initEnv taskList

    runNodeT env0 $ do
      void $ async $ forever $ do
        liftIO $ threadDelay $ 100 * 1000 * 1000
        checkHealth
      void $ async startMainLoop
      m

close :: MonadUnliftIO m => WorkerT m ()
close = stopNodeT

ping :: MonadUnliftIO m => WorkerT m Bool
ping = withAgentT $ do
  send Ping
  ret <- receive
  case ret of
    Left _     -> return False
    Right Pong -> return True
    Right _    -> return False

addFunc
  :: MonadUnliftIO m
  => FuncName -> JobT m () -> WorkerT m ()
addFunc f j = do
  withAgentT $ send (CanDo f)
  ref <- env
  HM.insert ref f j

broadcast
  :: MonadUnliftIO m
  => FuncName -> JobT m () -> WorkerT m ()
broadcast f j = do
  withAgentT $ send (Broadcast f)
  ref <- env
  HM.insert ref f j

removeFunc
  :: MonadUnliftIO m
  => FuncName -> WorkerT m ()
removeFunc f = do
  withAgentT $ send (CantDo f)
  ref <- env
  HM.delete ref f

grabJob
  :: MonadUnliftIO m
  => AgentEnv -> WorkerT m (Maybe Job)
grabJob agentEnv = do
  pl <- liftC . runAgentT agentEnv $ do
    size <- readerSize
    when (size == 0) $ send GrabJob
    timeout 10000000 receive

  case pl of
    Nothing                      -> pure Nothing
    Just (Right (JobAssign job)) -> pure (Just job)
    _                            -> pure Nothing


work
  :: MonadUnliftIO m
  => Int -> WorkerT m ()
work size = do
  asyncs <- replicateM size $ async work_
  void $ waitAnyCancel asyncs

work_ :: MonadUnliftIO m => WorkerT m ()
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
            catch task' $ \(e :: SomeException) -> do
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
  :: MonadUnliftIO m
  => WorkerT m ()
checkHealth = do
  ret <- timeout 10000000 ping
  case ret of
    Nothing -> close
    Just r  -> unless r close
