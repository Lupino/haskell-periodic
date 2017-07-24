{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Worker
  (
    Worker
  , runWorker
  , ping
  , addFunc
  , removeFunc
  , work
  , close
  ) where

import           Control.Concurrent      (forkIO)
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString         as B (empty)
import           Periodic.Agent          (receive, send)
import           Periodic.Job            (Job, JobEnv (..), func, initJobEnv,
                                          name, workFail)
import           Periodic.Timer
import           Periodic.Transport      (Transport)
import           Periodic.Types.Command
import           Periodic.Types.Error
import           Periodic.Types.Payload

import           Periodic.Types          (ClientType (TypeWorker), FuncName)

import           Periodic.IOHashMap      (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap      as HM (delete, insert, lookup)

import           Control.Concurrent.QSem
import           Control.Exception       (SomeException)
import           Control.Exception       (throwIO)
import           Control.Monad           (forever, void)
import           Periodic.Monad

import           System.Log.Logger       (errorM)
import           System.Timeout          (timeout)

type TaskList = IOHashMap (Job ())
type Worker = GenPeriodic TaskList

close :: Worker ()
close = stopPeriodic

runWorker :: Transport -> Worker a -> IO a
runWorker transport m = do
  timer <- newTimer
  taskList <- newIOHashMap
  env0 <- initEnv transport taskList TypeWorker
  runPeriodic env0 $ do
    wapperIO (initTimer timer) checkHealth
    liftIO $ repeatTimer' timer 100
    m

ping :: Worker Bool
ping = withAgent $ \agent -> do
  send agent Ping B.empty
  ret <- receive agent
  return $ payloadCMD ret == Pong

grabJob :: Worker (Maybe JobEnv)
grabJob  = do
  pl <- withAgent $ \agent -> do
          send agent GrabJob B.empty
          receive agent

  case payloadCMD pl of
    JobAssign -> return . initJobEnv $ payloadData pl
    Noop      -> liftIO . throwIO $ payloadError pl
    _         -> grabJob

addFunc :: FuncName -> Job () -> Worker ()
addFunc f j = do
  withAgent $ \agent -> send agent CanDo f
  ref <- userEnv
  liftIO $ HM.insert ref f j

removeFunc :: FuncName -> Worker ()
removeFunc f = do
  withAgent $ \agent -> send agent CantDo f
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
              void . wapperIO forkIO $ do
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

                liftIO $ signalQSem sem

checkHealth :: Worker ()
checkHealth = do
  ret <- wapperIO (timeout 10000000) ping
  case ret of
    Nothing -> noopAgent TransportTimeout
    Just r ->
      if r then return ()
           else noopAgent TransportClosed
