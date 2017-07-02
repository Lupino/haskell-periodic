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
  , close
  ) where

import           Data.ByteString.Char8  (ByteString)
import qualified Data.ByteString.Char8  as B (empty, hGet, hPut, length)
import           Periodic.Agent         (Agent, receive, send)
import           Periodic.BaseClient    (BaseClient, close, newBaseClient,
                                         noopAgent, withAgent)
import           Periodic.Socket        (Socket)
import           Periodic.Timer
import           Periodic.Types         (ClientType (TypeClient))
import           Periodic.Types.Command
import           Periodic.Types.Error
import           Periodic.Types.Job
import           Periodic.Types.Payload

import           Data.Int               (Int64)

import           Control.Exception      (catch, throwIO)
import           Control.Monad          (forever)
import           GHC.IO.Handle          (Handle)
import           Periodic.Utils         (getEpochTime, makeHeader, parseHeader)
import           System.Timeout         (timeout)

type Client = BaseClient

newClient :: Socket -> IO Client
newClient sock = do
  c <- newBaseClient sock TypeClient
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
  send agent SubmitJob (unparseJob j)
  isSuccess agent

submitJob :: Client -> FuncName -> JobName -> Int64 -> IO Bool
submitJob c jFuncName jName later = do

  jSchedAt <- (+later) <$> getEpochTime
  submitJob_ c $ Job { jWorkload = "", jCount = 0, .. }

dropFunc :: Client -> ByteString -> IO Bool
dropFunc c func = withAgent c $ \agent -> do
  send agent DropFunc func
  isSuccess agent

removeJob_ :: Client -> Job -> IO Bool
removeJob_ c j = withAgent c $ \agent -> do
  send agent RemoveJob (unparseJob j)
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

  where go :: Agent -> IO ()
        go agent = do
          header <- B.hGet h 4
          if header == B.empty then throwIO EmptyError
                               else pushData agent $ parseHeader header

        pushData :: Agent -> Int -> IO ()
        pushData agent len = do
          dat <- B.hGet h len
          send agent Load dat

checkHealth :: Client -> IO ()
checkHealth c = do
  ret <- timeout 10000000 $ ping c
  case ret of
    Nothing -> noopAgent c SocketTimeout
    Just r ->
      if r then return ()
           else noopAgent c SocketClosed
