{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Client
  (
    Client
  , Connection
  , open
  , close
  , runClient_
  , runClient

  , ping
  , submitJob_
  , submitJob
  , removeJob_
  , removeJob
  , dropFunc
  , status
  , shutdown
  ) where

import           Control.Concurrent           (forkIO)
import           Data.Byteable                (toBytes)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B (lines, split)
import           Periodic.Agent               (Agent, receive, receive_, send)
import           Periodic.Connection          (newClientConn)
import qualified Periodic.Connection          as Conn (receive, send)
import           Periodic.Socket              (connect)
import           Periodic.Timer
import           Periodic.Transport           (Transport, makeSocketTransport)
import           Periodic.Types               (ClientType (TypeClient))
import           Periodic.Types.ClientCommand
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand

import           Data.Int                     (Int64)

import           Control.Monad                (unless, void)
import           Periodic.Utils               (getEpochTime)

import           Periodic.Monad               hiding (catch)
import           System.Timeout               (timeout)

type Client     = GenPeriodic ()
type Connection = SpecEnv ()

open :: (Transport -> IO Transport) -> String -> IO Connection
open f h = runClient f h specEnv

close :: Client ()
close = stopPeriodic

runClient :: (Transport -> IO Transport) -> String -> Client a -> IO a
runClient f h m = do
  timer <- newTimer
  transport <- f =<< makeSocketTransport =<< connect h
  c <- newClientConn transport
  Conn.send c $ toBytes TypeClient
  void $ Conn.receive c
  env0 <- initEnv c ()
  runPeriodic env0 $ do
    wapperIO (repeatTimer' timer 100) checkHealth
    void . wapperIO forkIO . startMainLoop $ pure ()
    m

runClient_ :: Connection -> Client a -> IO a
runClient_ = runPeriodicWithSpecEnv

ping :: Client Bool
ping = withAgent $ \agent -> do
  send agent Ping
  ret <- receive agent
  case ret of
    Left _     -> return False
    Right Pong -> return True
    Right _    -> return False

submitJob_ :: Job -> Client Bool
submitJob_ j = withAgent $ \agent -> do
  send agent (SubmitJob j)
  isSuccess agent

submitJob :: FuncName -> JobName -> Int64 -> Client Bool
submitJob jFuncName jName later = do

  jSchedAt <- (+later) <$> unsafeLiftIO getEpochTime
  submitJob_ Job{jWorkload = "", jCount = 0, ..}

dropFunc :: FuncName -> Client Bool
dropFunc func = withAgent $ \agent -> do
  send agent (DropFunc func)
  isSuccess agent

removeJob_ :: Job -> Client Bool
removeJob_ j = withAgent $ \agent -> do
  send agent (RemoveJob j)
  isSuccess agent

removeJob :: FuncName -> JobName -> Client Bool
removeJob f n = removeJob_ $ newJob f n

isSuccess :: Agent -> IO Bool
isSuccess agent = do
  ret <- receive agent
  case ret of
    Left _        -> return False
    Right Success -> return True
    Right _       -> return False

status :: Client [[ByteString]]
status = withAgent $ \agent -> do
  send agent Status
  ret <- receive_ agent
  return . map (B.split ',') $ B.lines ret

shutdown :: Client ()
shutdown = withAgent $ \agent ->
  send agent Shutdown

checkHealth :: Client ()
checkHealth = do
  ret <- wapperIO (timeout 10000000) ping
  case ret of
    Nothing -> close
    Just r ->
      unless r close
