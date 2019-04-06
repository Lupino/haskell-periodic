{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Periodic.Client
  (
    ClientT
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

import           Control.Concurrent           (threadDelay)
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
import           Periodic.Socket              (connect)
import           Periodic.Transport           (Transport, makeSocketTransport)
import           Periodic.Types               (ClientType (TypeClient))
import           Periodic.Types.ClientCommand
import           Periodic.Types.Internal      (ConfigKey (..), parseBinary)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand
import           Periodic.Utils               (getEpochTime)
import           UnliftIO

type ClientT m = NodeT () m

data ClientEnv = ClientEnv
  { nodeEnv :: NodeEnv ()
  , connEnv :: ConnEnv
  }

runClientT :: Monad m => ClientEnv -> ClientT m a -> m a
runClientT ClientEnv{..} = runConnectionT connEnv . runNodeT nodeEnv

open
  :: MonadUnliftIO m
  => (Transport -> IO Transport) -> String -> m ClientEnv
open f h = do
  connEnv <- liftIO $
    initClientConnEnv
      =<< f
      =<< makeSocketTransport
      =<< connect h
  runConnectionT connEnv $ do
    Conn.send $ toBytes TypeClient
    void Conn.receive

  nodeEnv <- liftIO $ initEnv ()

  let clientEnv = ClientEnv{..}

  runClientT clientEnv $ do
    void . async $ forever $ do
      liftIO $ threadDelay $ 100 * 1000 * 1000
      checkHealth

    void $ async startMainLoop
  return clientEnv

close :: MonadUnliftIO m => ClientT m ()
close = stopNodeT

ping :: MonadUnliftIO m => ClientT m Bool
ping = withAgentT $ do
  send Ping
  ret <- receive
  case ret of
    Left _     -> return False
    Right Pong -> return True
    Right _    -> return False

submitJob_ :: MonadUnliftIO m => Job -> ClientT m Bool
submitJob_ j = withAgentT $ do
  send (SubmitJob j)
  isSuccess

submitJob
  :: MonadUnliftIO m
  => FuncName -> JobName -> Maybe Workload -> Maybe Int64 -> ClientT m Bool
submitJob fn jn w later = do
  schedAt <- (+fromMaybe 0 later) <$> liftIO getEpochTime
  submitJob_ $ setSchedAt schedAt $ setWorkload (fromMaybe "" w) $ initJob fn jn

runJob_ :: MonadUnliftIO m => Job -> ClientT m ByteString
runJob_ j = withAgentT $ do
  send (RunJob $ setSchedAt 0 j)
  receive_

runJob
  :: MonadUnliftIO m
  => FuncName -> JobName -> Maybe Workload -> ClientT m ByteString
runJob fn jn w = do
  schedAt <- liftIO getEpochTime
  runJob_ $ setSchedAt schedAt $ setWorkload (fromMaybe "" w) $ initJob fn jn

dropFunc
  :: MonadUnliftIO m
  => FuncName -> ClientT m Bool
dropFunc func = withAgentT $ do
  send (DropFunc func)
  isSuccess

removeJob
  :: MonadUnliftIO m
  => FuncName -> JobName -> ClientT m Bool
removeJob f n = withAgentT $ do
  send (RemoveJob f n)
  isSuccess

isSuccess :: MonadUnliftIO m => AgentT m Bool
isSuccess = do
  ret <- receive
  case ret of
    Left _        -> return False
    Right Success -> return True
    Right _       -> return False

status
  :: MonadUnliftIO m
  => ClientT m ByteString
status = withAgentT $ do
  send Status
  receive_

configGet
  :: MonadUnliftIO m
  => String -> ClientT m Int
configGet k = withAgentT $ do
  send (ConfigGet (ConfigKey k))
  ret <- receive
  case ret of
    Left _           -> return 0
    Right (Config v) -> return v
    Right _          -> return 0

configSet
  :: MonadUnliftIO m
  => String -> Int -> ClientT m Bool
configSet k v = withAgentT $ do
  send (ConfigSet (ConfigKey k) v)
  isSuccess

load :: MonadUnliftIO m => [Job] -> ClientT m Bool
load jobs = withAgentT $ do
  send (Load jobs)
  isSuccess

dump :: MonadUnliftIO m => ClientT m [Job]
dump = withAgentT $ do
  send Dump
  ret <- parseBinary <$> receive_
  case ret of
    Left _  -> return []
    Right v -> return v

shutdown :: MonadUnliftIO m => ClientT m ()
shutdown = withAgentT $ send Shutdown

checkHealth :: MonadUnliftIO m => ClientT m ()
checkHealth = do
  ret <- timeout 10000000 ping
  case ret of
    Nothing -> close
    Just r  -> unless r close
