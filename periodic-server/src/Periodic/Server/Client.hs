{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server.Client
  (
    Client
  , newClient
  , close
  , isAlive
  , startClient
  , runClient
  ) where

import           Control.Concurrent           (forkIO)
import           Control.Exception            (SomeException, throwIO, try)
import           Control.Monad                (forever, void, when)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B (concat, intercalate, pack)
import           Data.Foldable                (forM_)
import qualified Periodic.Connection          as Conn (Connection)

import           Periodic.Agent               (Agent, newAgent, receive, send,
                                               send_)
import           Periodic.Server.FuncStat     (FuncStat (..))
import           Periodic.Server.Scheduler    (Scheduler, dropFunc, dumpJob,
                                               pushJob, removeJob, shutdown,
                                               status)
import           Periodic.Types.ClientCommand
import           Periodic.Types.Job           (Job, decodeJob, encodeJob)
import           Periodic.Types.ServerCommand

import           Periodic.Monad
import           Periodic.Timer
import           Periodic.Types.Error         (Error (EmptyError))
import           Periodic.Utils               (getEpochTime)

import           Data.Int                     (Int64)
import           Data.IORef                   (IORef, atomicModifyIORef',
                                               newIORef)

type Client = GenPeriodic (IORef Int64)
type Connection = SpecEnv (IORef Int64)

newClient :: Conn.Connection -> Scheduler -> IO Connection
newClient conn sched = do
  lastVist <- newIORef =<< getEpochTime
  env0 <- initEnv_ (handleAgent sched lastVist) conn lastVist
  runPeriodic env0 specEnv

runClient :: Connection -> Client a -> IO a
runClient = runPeriodicWithSpecEnv

startClient :: Connection -> IO ()
startClient env = runPeriodicWithSpecEnv env startMainLoop

close :: Client ()
close = stopPeriodic

handleAgent :: Scheduler -> IORef Int64 -> Agent -> IO ()
handleAgent sched lastVist agent = do
  t <- getEpochTime
  atomicModifyIORef' lastVist (const (t, ()))
  cmd <- receive agent :: IO (Either String ClientCommand)
  case cmd of
    Left e     -> throwIO EmptyError -- close client
    Right cmd' -> go agent cmd'
  where go :: Agent -> ClientCommand -> IO ()
        go agent (SubmitJob job) = handleSubmitJob sched agent job
        go agent Status          = handleStatus sched agent
        go agent Ping            = send agent Pong
        go agent (DropFunc fn)   = handleDropFunc sched agent fn
        go agent (RemoveJob job) = handleRemoveJob sched agent job
        go agent Dump            = handleDump sched agent
        go _ (Load dat)          = handleLoad sched dat
        go _ Shutdown            = handleShutdown sched

handleSubmitJob :: Scheduler -> Agent -> Job -> IO ()
handleSubmitJob sc ag j = do
  pushJob sc j
  send ag Success

handleStatus :: Scheduler -> Agent -> IO ()
handleStatus sc ag = do
  stats <- map go <$> status sc
  send_ ag $ B.intercalate "\n" stats

  where go :: FuncStat -> ByteString
        go FuncStat{..} = B.concat [ sFuncName
                                   , ","
                                   , B.pack $ show sWorker
                                   , ","
                                   , B.pack $ show sJob
                                   , ","
                                   , B.pack $ show sProcess
                                   , ","
                                   , B.pack $ show sSchedAt
                                   ]

handleDropFunc :: Scheduler -> Agent -> ByteString -> IO ()
handleDropFunc sc ag pl = do
  dropFunc sc pl
  send ag Success

handleRemoveJob :: Scheduler -> Agent -> Job -> IO ()
handleRemoveJob sc ag job = do
  removeJob sc job
  send ag Success

handleDump :: Scheduler -> Agent -> IO ()
handleDump sc ag = do
  jobs <- dumpJob sc
  mapM_ doSend jobs
  send_ ag "EOF"

  where doSend :: Job -> IO ()
        doSend = send_ ag . encodeJob

handleLoad :: Scheduler -> ByteString -> IO ()
handleLoad sc pl = forM_ (decodeJob pl) (pushJob sc)

handleShutdown :: Scheduler -> IO ()
handleShutdown = shutdown
