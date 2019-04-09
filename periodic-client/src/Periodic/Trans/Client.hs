{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Periodic.Trans.Client
  ( ClientT
  , ClientEnv
  , open
  , close
  , runClientT

  , ping
  , submitJob_
  , submitJob
  , runJob_
  , runJob
  , removeJob
  , dropFunc
  , status
  , configGet
  , configSet
  , load
  , dump
  , shutdown
  ) where

import           Control.Monad                (forever, unless, void)
import           Data.Byteable                (toBytes)
import           Data.ByteString              (ByteString)
import           Data.Int                     (Int64)
import           Data.Maybe                   (fromMaybe)
import           Periodic.Agent               (AgentT, receive, receive_, send)
import           Periodic.Connection          (ConnEnv, initClientConnEnv,
                                               runConnectionT)
import qualified Periodic.Connection          as Conn
import           Periodic.Node
import           Periodic.Transport           (Transport, TransportConfig)
import           Periodic.Types               (ClientType (TypeClient))
import           Periodic.Types.ClientCommand
import           Periodic.Types.Internal      (ConfigKey (..), parseBinary)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand
import           Periodic.Utils               (getEpochTime)
import           UnliftIO
import           UnliftIO.Concurrent          (threadDelay)

type ClientT tp m = NodeT () tp m

data ClientEnv tp = ClientEnv
  { nodeEnv :: NodeEnv ()
  , connEnv :: ConnEnv tp
  }

runClientT :: Monad m => ClientEnv tp -> ClientT tp m a -> m a
runClientT ClientEnv{..} = runConnectionT connEnv . runNodeT nodeEnv

open
  :: (MonadUnliftIO m, Transport tp)
  => TransportConfig tp -> m (ClientEnv tp)
open config = do
  connEnv <- initClientConnEnv config
  runConnectionT connEnv $ do
    Conn.send $ toBytes TypeClient
    void Conn.receive

  nodeEnv <- initEnv ()

  let clientEnv = ClientEnv{..}

  runClientT clientEnv $ do
    void . async $ forever $ do
      threadDelay $ 100 * 1000 * 1000
      checkHealth

    void $ async startMainLoop
  return clientEnv

close :: (MonadUnliftIO m, Transport tp) => ClientT tp m ()
close = stopNodeT

ping :: (MonadUnliftIO m, Transport tp) => ClientT tp m Bool
ping = withAgentT $ do
  send Ping
  ret <- receive
  case ret of
    Left _     -> return False
    Right Pong -> return True
    Right _    -> return False

submitJob_ :: (MonadUnliftIO m, Transport tp) => Job -> ClientT tp m Bool
submitJob_ j = withAgentT $ do
  send (SubmitJob j)
  isSuccess

submitJob
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobName -> Maybe Workload -> Maybe Int64 -> ClientT tp m Bool
submitJob fn jn w later = do
  schedAt <- (+fromMaybe 0 later) <$> getEpochTime
  submitJob_ $ setSchedAt schedAt $ setWorkload (fromMaybe "" w) $ initJob fn jn

runJob_ :: (MonadUnliftIO m, Transport tp) => Job -> ClientT tp m ByteString
runJob_ j = withAgentT $ do
  send (RunJob $ setSchedAt 0 j)
  receive_

runJob
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobName -> Maybe Workload -> ClientT tp m ByteString
runJob fn jn w = do
  schedAt <- getEpochTime
  runJob_ $ setSchedAt schedAt $ setWorkload (fromMaybe "" w) $ initJob fn jn

dropFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> ClientT tp m Bool
dropFunc func = withAgentT $ do
  send (DropFunc func)
  isSuccess

removeJob
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobName -> ClientT tp m Bool
removeJob f n = withAgentT $ do
  send (RemoveJob f n)
  isSuccess

isSuccess :: (MonadUnliftIO m, Transport tp) => AgentT tp m Bool
isSuccess = do
  ret <- receive
  case ret of
    Left _        -> return False
    Right Success -> return True
    Right _       -> return False

status
  :: (MonadUnliftIO m, Transport tp)
  => ClientT tp m ByteString
status = withAgentT $ do
  send Status
  receive_

configGet
  :: (MonadUnliftIO m, Transport tp)
  => String -> ClientT tp m Int
configGet k = withAgentT $ do
  send (ConfigGet (ConfigKey k))
  ret <- receive
  case ret of
    Left _           -> return 0
    Right (Config v) -> return v
    Right _          -> return 0

configSet
  :: (MonadUnliftIO m, Transport tp)
  => String -> Int -> ClientT tp m Bool
configSet k v = withAgentT $ do
  send (ConfigSet (ConfigKey k) v)
  isSuccess

load :: (MonadUnliftIO m, Transport tp) => [Job] -> ClientT tp m Bool
load jobs = withAgentT $ do
  send (Load jobs)
  isSuccess

dump :: (MonadUnliftIO m, Transport tp) => ClientT tp m [Job]
dump = withAgentT $ do
  send Dump
  ret <- parseBinary <$> receive_
  case ret of
    Left _  -> return []
    Right v -> return v

shutdown :: (MonadUnliftIO m, Transport tp) => ClientT tp m ()
shutdown = withAgentT $ send Shutdown

checkHealth :: (MonadUnliftIO m, Transport tp) => ClientT tp m ()
checkHealth = do
  ret <- timeout 10000000 ping
  case ret of
    Nothing -> close
    Just r  -> unless r close
