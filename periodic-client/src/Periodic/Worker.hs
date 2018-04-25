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

import           Control.Monad.Catch             (MonadCatch, MonadMask, catch)
import           Control.Monad.IO.Class          (MonadIO (..))
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Data.Byteable                   (toBytes)
import           Periodic.Agent                  (AgentEnv, readerSize, receive,
                                                  runAgentT, send)
import           Periodic.Job                    (JobConfig, JobT, func_,
                                                  initJobConfig, name, workFail)
import           Periodic.Socket                 (connect)
import           Periodic.Transport              (Transport,
                                                  makeSocketTransport)
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand

import           Periodic.Types                  (ClientType (TypeWorker),
                                                  FuncName)

import           Periodic.Connection             (initClientConnEnv)
import qualified Periodic.Connection             as Conn
import           Periodic.IOHashMap              (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap              as HM (delete, insert, lookup)

import           Control.Concurrent.Async.Lifted (async, waitAnyCancel)
import           Control.Exception               (SomeException)
import           Control.Monad                   (forever, replicateM, void,
                                                  when)
import           Periodic.Node

import           System.Log.Logger               (errorM)
import           System.Timeout.Lifted           (timeout)

import           Data.Maybe                      (fromJust, isJust)

type TaskList m = IOHashMap FuncName (JobT m ())
type WorkerT m = NodeT (TaskList m) m

runWorkerT
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m, MonadMask m)
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

    taskList <- liftIO newIOHashMap
    env0 <- liftIO $ initEnv taskList

    runNodeT env0 $ do
      void $ async startMainLoop
      m

close :: MonadIO m => WorkerT m ()
close = stopNodeT

ping :: (MonadIO m, MonadMask m) => WorkerT m Bool
ping = withAgentT $ do
  send Ping
  ret <- receive
  case ret of
    Left _     -> return False
    Right Pong -> return True
    Right _    -> return False

addFunc
  :: (MonadIO m, MonadMask m)
  => FuncName -> JobT m () -> WorkerT m ()
addFunc f j = do
  withAgentT $ send (CanDo f)
  ref <- env
  liftIO $ HM.insert ref f j

broadcast
  :: (MonadIO m, MonadMask m)
  => FuncName -> JobT m () -> WorkerT m ()
broadcast f j = do
  withAgentT $ send (Broadcast f)
  ref <- env
  liftIO $ HM.insert ref f j

removeFunc
  :: (MonadIO m, MonadMask m)
  => FuncName -> WorkerT m ()
removeFunc f = do
  withAgentT $ send (CantDo f)
  ref <- env
  liftIO $ HM.delete ref f

grabJob
  :: (MonadIO m, MonadMask m, MonadBaseControl IO m)
  => AgentEnv -> WorkerT m (Maybe JobConfig)
grabJob agentEnv = do
  pl <- liftC . runAgentT agentEnv $ do
    size <- readerSize
    when (size == 0) $ send GrabJob
    timeout 10000000 receive

  case pl of
    Nothing                         -> pure Nothing
    Just (Right (JobAssign jh job)) -> pure (Just $ initJobConfig job jh)
    _                               -> pure Nothing


work
  :: (MonadMask m, MonadIO m, MonadBaseControl IO m)
  => Int -> WorkerT m ()
work size = do
  asyncs <- replicateM size $ async work_
  void . liftIO $ waitAnyCancel asyncs

work_
  :: (MonadMask m, MonadIO m, MonadBaseControl IO m)
  => WorkerT m ()
work_ = do
  taskList <- env
  agentEnv <- newAgentEnv
  forever $ do
    j <- grabJob agentEnv
    when (isJust j) $
      withEnv (fromJust j) $ do
        f <- func_
        task <- liftIO $ HM.lookup taskList f
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
