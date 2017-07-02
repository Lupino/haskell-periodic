{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Worker
  (
    Worker
  , newWorker
  , ping
  , addFunc
  , removeFunc
  , grabJob
  , work
  , close
  ) where

import           Data.ByteString.Char8   (ByteString)
import qualified Data.ByteString.Char8   as B (empty, unpack)
import           Periodic.Agent          (receive, send)
import           Periodic.BaseClient     (BaseClient, newBaseClient, noopAgent,
                                          withAgent)
import qualified Periodic.BaseClient     as BC (close)
import           Periodic.Job            (Job, func, name, newJob, workFail)
import           Periodic.Socket         (Socket)
import           Periodic.Timer
import           Periodic.Types.Command
import           Periodic.Types.Error
import           Periodic.Types.Payload

import           Periodic.Types          (ClientType (TypeWorker))

import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM (delete, empty, insert, lookup)
import           Data.IORef              (IORef, atomicModifyIORef', newIORef)

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.QSem
import           Control.Exception       (SomeException, catch, handle)
import           Control.Exception       (throwIO)
import           Control.Monad           (forever, void)
import           System.Timeout          (timeout)

import           System.Log.Logger       (errorM)

newtype Task = Task (Job -> IO ())

data Worker = Worker { bc    :: BaseClient
                     , tasks :: IORef (HashMap ByteString Task)
                     }


newWorker :: Socket -> IO Worker
newWorker sock = do
  bc <- newBaseClient sock TypeWorker
  tasks <- newIORef HM.empty
  let w = Worker { .. }

  timer <- newTimer
  initTimer timer $ checkHealth w
  repeatTimer' timer 100

  return w

addTask :: Worker -> ByteString -> Task -> IO ()
addTask w f t = atomicModifyIORef' (tasks w) $ \v -> (HM.insert f t v, ())

removeTask :: Worker -> ByteString -> IO ()
removeTask w f = atomicModifyIORef' (tasks w) $ \v -> (HM.delete f v, ())

getTask :: Worker -> ByteString -> IO (Maybe Task)
getTask w f = atomicModifyIORef' (tasks w) $ \v -> (v, HM.lookup f v)

ping :: Worker -> IO Bool
ping (Worker { bc = c }) = withAgent c $ \agent -> do
  send agent Ping B.empty
  ret <- receive agent
  return $ payloadCMD ret == Pong

grabJob :: Worker -> IO (Maybe Job)
grabJob w@(Worker { bc = c }) = withAgent c $ \agent -> do
  send agent GrabJob B.empty
  pl <- receive agent
  case payloadCMD pl of
    JobAssign -> return . newJob c $ payloadData pl
    Noop      -> throwIO $ payloadError pl
    _         -> grabJob w

addFunc :: Worker -> ByteString -> (Job -> IO ()) -> IO ()
addFunc w@(Worker { bc = c }) f t = withAgent c $ \agent -> do
  send agent CanDo f
  addTask w f (Task t)

removeFunc :: Worker -> ByteString -> IO ()
removeFunc w@(Worker { bc = c }) f = withAgent c $ \agent -> do
  send agent CantDo f
  removeTask w f

close :: Worker -> IO ()
close (Worker { bc = c }) = BC.close c

work :: Worker -> Int -> IO ()
work w size = handle (\(_ :: Error) -> close w) $ do
  sem <- newQSem size
  forever $ do
    j <- grabJob w
    case j of
      Nothing -> errorM "Periodic.Worker" "Nothing Job"
      Just job -> do
        task <- getTask w (func job)
        case task of
          Nothing -> removeFunc w (func job)
          Just task' -> do
            waitQSem sem
            void . forkIO $ runTask task' job >> signalQSem sem

runTask :: Task -> Job -> IO ()
runTask (Task task) job = catch (task job) $ \(e :: SomeException) -> do
  errorM "Periodic.Worker" $ concat [ "Failing on running job { name = "
                                    , B.unpack $ name job
                                    , ", "
                                    , B.unpack $ func job
                                    , " }"
                                    , "\nError: "
                                    , show e
                                    ]
  workFail job

checkHealth :: Worker -> IO ()
checkHealth w = do
  ret <- timeout 10000000 $ ping w
  case ret of
    Nothing -> noopAgent (bc w) SocketTimeout
    Just r ->
      if r then return ()
           else noopAgent (bc w) SocketClosed
