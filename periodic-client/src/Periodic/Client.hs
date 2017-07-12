{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Client
  (
    Client
  , newClient
  , ping
  , submitJob_
  , submitJob
  , removeJob_
  , removeJob
  , dropFunc
  , dump
  , load
  , status
  , close
  ) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B (empty, hGet, hPut, length)
import qualified Data.ByteString.Char8  as B (lines, split)
import           Periodic.Agent         (Agent, receive, send)
import           Periodic.BaseClient    (BaseClient, close, newBaseClient,
                                         noopAgent, withAgent)
import           Periodic.Timer
import           Periodic.Transport     (Transport)
import           Periodic.Types         (ClientType (TypeClient))
import           Periodic.Types.Command
import           Periodic.Types.Error
import           Periodic.Types.Job
import           Periodic.Types.Payload

import           Data.Int               (Int64)

import           Control.Exception      (catch, throwIO)
import           Control.Monad          (forever)
import           GHC.IO.Handle          (Handle, hClose)
import           Periodic.Utils         (getEpochTime, makeHeader, parseHeader)
import           System.Timeout         (timeout)

type Client = BaseClient

newClient :: Transport -> IO Client
newClient transport = do
  c <- newBaseClient transport TypeClient
  timer <- newTimer
  initTimer timer $ checkHealth c
  repeatTimer' timer 100
  return c

ping :: Client -> IO Bool
ping c = withAgent c $ \agent -> do
  send agent Ping B.empty
  ret <- receive agent
  return $ payloadCMD ret == Pong

submitJob_ :: Client -> Job -> IO Bool
submitJob_ c j = withAgent c $ \agent -> do
  send agent SubmitJob (encodeJob j)
  isSuccess agent

submitJob :: Client -> FuncName -> JobName -> Int64 -> IO Bool
submitJob c jFuncName jName later = do

  jSchedAt <- (+later) <$> getEpochTime
  submitJob_ c $ Job { jWorkload = "", jCount = 0, .. }

dropFunc :: Client -> FuncName -> IO Bool
dropFunc c func = withAgent c $ \agent -> do
  send agent DropFunc func
  isSuccess agent

removeJob_ :: Client -> Job -> IO Bool
removeJob_ c j = withAgent c $ \agent -> do
  send agent RemoveJob (encodeJob j)
  isSuccess agent

removeJob :: Client -> FuncName -> JobName -> IO Bool
removeJob c f n = removeJob_ c $ newJob f n

isSuccess :: Agent -> IO Bool
isSuccess agent = do
  ret <- receive agent
  return $ payloadCMD ret == Success

dump :: Client -> Handle -> IO ()
dump c h = withAgent c $ \agent -> do
  send agent Dump B.empty
  catch (forever $ go agent) $ \(_ :: Error) -> return ()
  hClose h

  where go :: Agent -> IO ()
        go agent = do
          ret <- payloadData <$> receive agent
          if ret == "EOF" then throwIO EmptyError
                          else putData ret

        putData :: ByteString -> IO ()
        putData dat = do
          B.hPut h . makeHeader $ B.length dat
          B.hPut h dat

load :: Client -> Handle -> IO ()
load c h = withAgent c $ \agent -> do
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
          send agent Load dat

status :: Client -> IO [[ByteString]]
status c = withAgent c $ \agent -> do
  send agent Status B.empty
  ret <- receive agent
  return . map (B.split ',') $ B.lines $ payloadData ret

checkHealth :: Client -> IO ()
checkHealth c = do
  ret <- timeout 10000000 $ ping c
  case ret of
    Nothing -> noopAgent c TransportTimeout
    Just r ->
      if r then return ()
           else noopAgent c TransportClosed
