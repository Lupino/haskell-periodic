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
  , dump
  , load
  , status
  , shutdown
  ) where

import           Control.Monad                (void)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Byteable                (toBytes)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as B (empty, hGet, hPut, length)
import qualified Data.ByteString.Char8        as B (lines, split)
import           Periodic.Agent               (Agent, receive, receive_, send)
import           Periodic.Connection          (newClientConn)
import qualified Periodic.Connection          as Conn (receive, send)
import           Periodic.Socket              (connect)
import           Periodic.Timer
import           Periodic.Transport           (Transport, makeSocketTransport)
import           Periodic.Types               (ClientType (TypeClient))
import           Periodic.Types.ClientCommand
import           Periodic.Types.Error
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand

import           Data.Int                     (Int64)

import           Control.Exception            (catch, throwIO)
import           Control.Monad                (forever)
import           GHC.IO.Handle                (Handle, hClose)
import           Periodic.Utils               (getEpochTime, makeHeader,
                                               parseHeader)

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
  env0 <- initEnv (const $ pure ()) c ()
  runPeriodic env0 $ do
    wapperIO (initTimer timer) checkHealth
    liftIO $ repeatTimer' timer 100
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

  jSchedAt <- (+later) <$> liftIO getEpochTime
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

dump :: Handle -> Client ()
dump h = withAgent $ \agent -> do
  send agent Dump
  catch (forever $ go agent) $ \(_ :: Error) -> return ()
  hClose h

  where go :: Agent -> IO ()
        go agent = do
          ret <- receive_ agent
          if ret == "EOF" then throwIO EmptyError
                          else putData ret

        putData :: ByteString -> IO ()
        putData dat = do
          B.hPut h . makeHeader $ B.length dat
          B.hPut h dat

load :: Handle -> Client ()
load h = withAgent $ \agent -> do
  catch (forever $ go agent) $ \(_ :: Error) -> return ()
  hClose h

  where go :: Agent -> IO ()
        go agent = do
          header <- B.hGet h 4
          if header == B.empty then throwIO EmptyError
                               else pushData agent $ parseHeader header

        pushData :: Agent -> Int -> IO ()
        pushData agent len = do
          dat <- B.hGet h len
          send agent (Load dat)

status :: Client [[ByteString]]
status = withAgent $ \agent -> do
  send agent Status
  ret <- receive_ agent
  return . map (B.split ',') $ B.lines $ ret

shutdown :: Client ()
shutdown = withAgent $ \agent ->
  send agent Shutdown

checkHealth :: Client ()
checkHealth = do
  ret <- wapperIO (timeout 10000000) ping
  case ret of
    Nothing -> close
    Just r ->
      if r then return ()
           else close
