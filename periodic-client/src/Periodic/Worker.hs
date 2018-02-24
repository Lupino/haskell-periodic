{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Worker
  (
    Worker
  , runWorker
  , ping
  , addFunc
  , broadcast
  , removeFunc
  , work
  , close
  ) where

import           Control.Concurrent           (forkIO)
import           Data.Byteable                (toBytes)
import           Periodic.Agent               (Agent, receive, send)
import           Periodic.Job                 (Job, JobEnv (..), func_,
                                               initJobEnv, name, workFail)
import           Periodic.Socket              (connect)
import           Periodic.Transport           (Transport, makeSocketTransport)
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand

import           Periodic.Types               (ClientType (TypeWorker),
                                               FuncName)

import           Periodic.Connection          (newClientConn)
import qualified Periodic.Connection          as Conn (receive, send)
import           Periodic.IOHashMap           (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap           as HM (delete, insert, lookup)

import           Control.Concurrent.Async     (async, waitAnyCancel)
import           Control.Exception            (SomeException)
import           Control.Monad                (forever, replicateM, void, when)
import           Periodic.Monad

import           System.Log.Logger            (errorM)
import           System.Timeout               (timeout)

import           Data.Maybe                   (fromJust, isJust)

type TaskList = IOHashMap FuncName (Job ())
type Worker = GenPeriodic TaskList

close :: Worker ()
close = stopPeriodic

runWorker :: (Transport -> IO Transport) -> String -> Worker a -> IO a
runWorker f h m = do
  transport <- f =<< makeSocketTransport =<< connect h
  taskList <- newIOHashMap
  c <- newClientConn transport
  Conn.send c $ toBytes TypeWorker
  void $ Conn.receive c
  env0 <- initEnv c taskList
  runPeriodic env0 $ do
    void . wapperIO forkIO . startMainLoop $ pure ()
    m

ping :: Worker Bool
ping = withAgent $ \agent -> do
  send agent Ping
  ret <- receive agent
  case ret of
    Left _     -> return False
    Right Pong -> return True
    Right _    -> return False

grabJob :: Agent -> Worker (Maybe JobEnv)
grabJob agent = do
  unsafeLiftIO $ send agent GrabJob
  pl <- unsafeLiftIO . timeout 10000000 $ receive agent

  case pl of
    Nothing                         -> pure Nothing
    Just (Right (JobAssign jh job)) -> pure (Just $ initJobEnv job jh)
    _                               -> pure Nothing

addFunc :: FuncName -> Job () -> Worker ()
addFunc f j = do
  withAgent $ \agent -> send agent (CanDo f)
  ref <- userEnv
  unsafeLiftIO $ HM.insert ref f j

broadcast :: FuncName -> Job () -> Worker ()
broadcast f j = do
  withAgent $ \agent -> send agent (Broadcast f)
  ref <- userEnv
  unsafeLiftIO $ HM.insert ref f j

removeFunc :: FuncName -> Worker ()
removeFunc f = do
  withAgent $ \agent -> send agent (CantDo f)
  ref <- userEnv
  unsafeLiftIO $ HM.delete ref f

work :: Int -> Worker ()
work size = do
  asyncs <- replicateM size $ wapperIO async work_
  void . unsafeLiftIO $ waitAnyCancel asyncs

work_ :: Worker ()
work_ = do
  env0 <- env
  taskList <- userEnv
  agent <- newEmptyAgent
  forever $ do
    j <- grabJob agent
    when (isJust j) $ do
      env1 <- cloneEnv $ fromJust j
      withEnv env1 $ do
        f <- func_
        task <- unsafeLiftIO $ HM.lookup taskList f
        case task of
          Nothing -> do
            withEnv env0 $ removeFunc f
            workFail
          Just task' ->
            catch task' $ \(e :: SomeException) -> do
              n <- name
              unsafeLiftIO $ errorM "Periodic.Worker"
                     $ concat [ "Failing on running job { name = "
                              , n
                              , ", "
                              , show f
                              , " }"
                              , "\nError: "
                              , show e
                              ]
              workFail
