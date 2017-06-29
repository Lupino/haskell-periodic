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
  , Job (..)
  , job
  ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (empty, hGet, hPut, length)
import           Data.ByteString.Lazy  (toStrict)
import           Periodic.Agent        (Agent, receive, send)
import           Periodic.BaseClient   (BaseClient, close, newBaseClient,
                                        noopAgent, withAgent)
import           Periodic.Socket       (Socket)
import           Periodic.Types        (ClientType (TypeClient), Command (..),
                                        Error (..), Payload (..))

import           Data.Aeson            (ToJSON (..), encode, object, (.=))
import           Data.Int              (Int64)

import           Control.Concurrent    (forkIO, threadDelay)

import           Control.Exception     (catch, throwIO)
import           Control.Monad         (forever, void)
import           GHC.IO.Handle         (Handle)
import           Periodic.Utils        (getEpochTime, makeHeader, parseHeader)
import           System.Timeout        (timeout)

type Client = BaseClient

data Job = Job { name     :: String
               -- unique job name
               , func     :: String
               -- refer worker func
               , workload :: String
               , schedAt  :: Int64
               }
  deriving (Show)

instance ToJSON Job where
  toJSON Job{..} = object [ "name"     .= name
                          , "func"     .= func
                          , "workload" .= workload
                          , "sched_at" .= schedAt
                          ]

job :: String -> String -> Job
job func name = Job { workload = ""
                    , schedAt  = 0
                    , ..
                    }

newClient :: Socket -> IO Client
newClient sock = do
  c <- newBaseClient sock TypeClient
  void $ forkIO $ forever $ checkHealth c
  return c

ping :: Client -> IO Bool
ping c = withAgent c $ \agent -> do
  send agent Ping B.empty
  ret <- receive agent
  return $ payloadCMD ret == Pong

submitJob_ :: Client -> Job -> IO Bool
submitJob_ c j = withAgent c $ \agent -> do
  send agent SubmitJob (toStrict $ encode j)
  isSuccess agent

submitJob :: Client -> String -> String -> Int64 -> IO Bool
submitJob c func name later = do

  schedAt <- (+later) <$> getEpochTime
  submitJob_ c $ Job { workload = "", .. }

dropFunc :: Client -> ByteString -> IO Bool
dropFunc c func = withAgent c $ \agent -> do
  send agent DropFunc func
  isSuccess agent

removeJob_ :: Client -> Job -> IO Bool
removeJob_ c j = withAgent c $ \agent -> do
  send agent RemoveJob (toStrict $ encode j)
  isSuccess agent

removeJob :: Client -> String -> String -> IO Bool
removeJob c f n = removeJob_ c $ job f n

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
      if r then threadDelay 10000000
           else noopAgent c SocketClosed
