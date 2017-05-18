{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Client
  (
    Client
  , newClient
  , ping
  , submitJob
  , removeJob
  , dropFunc
  , close
  , Job (..)
  , job
  , job'
  ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (empty)
import           Data.ByteString.Lazy  (toStrict)
import           Network               (HostName, PortID)
import           Periodic.Agent        (Agent, receive, send)
import           Periodic.BaseClient   (BaseClient, close, connectTo,
                                        newBaseClient, withAgent)
import           Periodic.Types        (ClientType (TypeClient), Command (..),
                                        Payload (..))

import           Data.Aeson            (ToJSON (..), encode, object, (.=))
import           Data.Int              (Int64)

import           Data.UnixTime

type Client = BaseClient

data Job = Job { name     :: String
               -- unique job name
               , func     :: String
               -- refer worker func
               , workload :: String
               , timeout  :: Int64
               , schedAt  :: Int64
               }
  deriving (Show)

instance ToJSON Job where
  toJSON Job{..} = object [ "name"     .= name
                          , "func"     .= func
                          , "workload" .= workload
                          , "timeout"  .= timeout
                          , "sched_at" .= schedAt
                          ]

job :: String -> String -> Job
job func name = Job { workload = ""
                    , timeout  = 0
                    , schedAt  = 0
                    , ..
                    }

job' :: String -> String -> Int64 -> IO Job
job' func name later = do
  schedAt <- (+later) . read . show . toEpochTime <$> getUnixTime

  return Job { workload = ""
             , timeout = 0
             , ..
             }

newClient :: HostName -> PortID -> IO Client
newClient host portID = do
  sock <- connectTo host portID
  newBaseClient sock TypeClient

ping :: Client -> IO Bool
ping c = withAgent c $ \agent -> do
  send agent Ping B.empty
  ret <- receive agent
  return $ payloadCMD ret == Pong

submitJob :: Client -> Job -> IO Bool
submitJob c j = withAgent c $ \agent -> do
  send agent SubmitJob (toStrict $ encode j)
  isSuccess agent

dropFunc :: Client -> ByteString -> IO Bool
dropFunc c func = withAgent c $ \agent -> do
  send agent DropFunc func
  isSuccess agent

removeJob :: Client -> Job -> IO Bool
removeJob c j = withAgent c $ \agent -> do
  send agent RemoveJob (toStrict $ encode j)
  isSuccess agent

isSuccess :: Agent -> IO Bool
isSuccess agent = do
  ret <- receive agent
  return $ payloadCMD ret == Success
