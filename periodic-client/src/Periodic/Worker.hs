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

import           Control.Concurrent           (forkFinally, forkIO)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Byteable                (toBytes)
import qualified Data.ByteString              as B (empty)
import           Periodic.Agent               (receive, send)
import           Periodic.Job                 (Job, JobEnv (..), func,
                                               initJobEnv, name, workFail)
import           Periodic.Socket              (connect)
import           Periodic.Timer
import           Periodic.Transport           (Transport, makeSocketTransport)
import           Periodic.Types.Error
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand

import           Periodic.Types               (ClientType (TypeWorker),
                                               FuncName)

import           Periodic.Connection          (newClientConn)
import qualified Periodic.Connection          as Conn (receive, send)
import           Periodic.IOHashMap           (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap           as HM (delete, insert, lookup)

import           Control.Concurrent.QSem
import           Control.Exception            (SomeException, throwIO)
import           Control.Monad                (forever, unless, void)
import           Periodic.Monad

import           System.Log.Logger            (errorM)
import           System.Timeout               (timeout)

type TaskList = IOHashMap FuncName (Job ())
type Worker = GenPeriodic TaskList

close :: Worker ()
close = stopPeriodic

runWorker :: (Transport -> IO Transport) -> String -> Worker a -> IO a
runWorker f h m = do
  timer <- newTimer
  transport <- f =<< makeSocketTransport =<< connect h
  taskList <- newIOHashMap
  c <- newClientConn transport
  Conn.send c $ toBytes TypeWorker
  void $ Conn.receive c
  env0 <- initEnv c taskList
  runPeriodic env0 $ do
    wapperIO (repeatTimer' timer 100) checkHealth
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

grabJob :: Worker (Maybe JobEnv)
grabJob  = do
  pl <- withAgent $ \agent -> do
          send agent GrabJob
          receive agent

  case pl of
    Left _                   -> pure Nothing
    Right (JobAssign jh job) -> return $ Just (initJobEnv job jh)
    _                        -> grabJob

addFunc :: FuncName -> Job () -> Worker ()
addFunc f j = do
  withAgent $ \agent -> send agent (CanDo f)
  ref <- userEnv
  liftIO $ HM.insert ref f j

broadcast :: FuncName -> Job () -> Worker ()
broadcast f j = do
  withAgent $ \agent -> send agent (Broadcast f)
  ref <- userEnv
  liftIO $ HM.insert ref f j

removeFunc :: FuncName -> Worker ()
removeFunc f = do
  withAgent $ \agent -> send agent (CantDo f)
  ref <- userEnv
  liftIO $ HM.delete ref f

work :: Int -> Worker ()
work size = do
  env0 <- env
  taskList <- userEnv
  sem <- liftIO $ newQSem size
  forever $ do
    j <- grabJob
    case j of
      Nothing -> liftIO $ errorM "Periodic.Worker" "Nothing Job"
      Just job -> do
        env1 <- cloneEnv job
        withEnv env1 $ do
          f <- func
          task <- liftIO $ HM.lookup taskList f
          case task of
            Nothing -> do
              withEnv env0 $ removeFunc f
              workFail
            Just task' -> do
              liftIO $ waitQSem sem
              void . wapperIO (flip forkFinally (const $ signalQSem sem)) $ do
                catch task' $ \(e :: SomeException) -> do
                  n <- name
                  liftIO $ errorM "Periodic.Worker"
                         $ concat [ "Failing on running job { name = "
                                  , show n
                                  , ", "
                                  , show f
                                  , " }"
                                  , "\nError: "
                                  , show e
                                  ]
                  workFail


checkHealth :: Worker ()
checkHealth = do
  ret <- wapperIO (timeout 10000000) ping
  case ret of
    Nothing -> close
    Just r ->
      unless r close
