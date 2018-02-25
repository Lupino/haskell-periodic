{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server.Client
  (
    Client
  , Connection
  , newClient
  , close
  , isAlive
  , startClient
  , runClient
  , getLastVist
  , clientId
  ) where

import           Data.Byteable                (toBytes)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B (intercalate)
import qualified Periodic.Connection          as Conn (Connection, connid)

import           Periodic.Agent               (Agent, receive, send, send_)
import           Periodic.Server.Scheduler    (Scheduler, dropFunc, dumpJob,
                                               pushJob, removeJob, shutdown,
                                               status)
import           Periodic.Types.ClientCommand
import           Periodic.Types.ServerCommand

import           Periodic.Monad
import           Periodic.Utils               (getEpochTime)

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM            (atomically)
import           Data.Int                     (Int64)

type Client = GenPeriodic (TVar Int64)
type Connection = SpecEnv (TVar Int64)

newClient :: Conn.Connection -> Scheduler -> IO Connection
newClient conn0 sched = do
  lastVist <- newTVarIO =<< getEpochTime
  env0 <- initEnv_ (handleAgent sched) conn0 lastVist
  runPeriodic env0 specEnv

runClient :: Connection -> Client a -> IO a
runClient = runPeriodicWithSpecEnv

startClient :: Connection -> IO () -> IO ()
startClient env0 = runPeriodicWithSpecEnv env0 . startMainLoop

close :: Client ()
close = stopPeriodic

getLastVist :: Client Int64
getLastVist = do
  ref <- userEnv
  unsafeLiftIO $ readTVarIO ref

clientId :: Client ByteString
clientId = Conn.connid . conn <$> env

handleAgent :: Scheduler -> Agent -> Client ()
handleAgent sched agent = do
  lastVist <- userEnv
  unsafeLiftIO $ do
    t <- getEpochTime
    atomically $ writeTVar lastVist t

  cmd <- unsafeLiftIO $ receive agent :: Client (Either String ClientCommand)
  case cmd of
    Left _     -> close -- close client
    Right (SubmitJob job) -> unsafeLiftIO $ do
      pushJob sched job
      send agent Success
    Right Status -> unsafeLiftIO $ do
      stats <- map toBytes <$> status sched
      send_ agent $ B.intercalate "\n" stats
    Right Ping -> unsafeLiftIO $ send agent Pong
    Right (DropFunc fn) -> unsafeLiftIO $ do
      dropFunc sched fn
      send agent Success
    Right (RemoveJob job) -> unsafeLiftIO $ do
      removeJob sched job
      send agent Success
    Right Shutdown -> unsafeLiftIO $ shutdown sched
