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
  , removeJob_
  , removeJob
  , dropFunc
  , status
  , configGet
  , configSet
  , load
  , dump
  , shutdown
  ) where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad.Catch          (MonadCatch, MonadMask)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Trans.Control  (MonadBaseControl,
                                               liftBaseDiscard)
import           Data.Byteable                (toBytes)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B (lines, split)
import           Periodic.Agent               (AgentT, receive, receive_, send)
import           Periodic.Connection          (ConnEnv, initClientConnEnv,
                                               runConnectionT)
import qualified Periodic.Connection          as Conn
import           Periodic.Socket              (connect)
import           Periodic.Transport           (Transport, makeSocketTransport)
import           Periodic.Types               (ClientType (TypeClient))
import           Periodic.Types.ClientCommand
import           Periodic.Types.Internal      (ConfigKey (..))
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand

import           Data.Int                     (Int64)
import           Data.Maybe                   (fromMaybe)

import           Control.Monad                (forever, unless, void)
import           Periodic.Utils               (getEpochTime)

import           Periodic.Node
import           System.Timeout.Lifted        (timeout)

type ClientT m = NodeT () m

data ClientEnv = ClientEnv
  { nodeEnv :: NodeEnv ()
  , connEnv :: ConnEnv
  }

runClientT :: Monad m => ClientEnv -> ClientT m a -> m a
runClientT ClientEnv{..} = runConnectionT connEnv . runNodeT nodeEnv

open
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m, MonadMask m)
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
    void . liftBaseDiscard forkIO $ forever $ do
      liftIO $ threadDelay $ 100 * 1000 * 1000
      checkHealth

    void $ liftBaseDiscard forkIO startMainLoop
  return clientEnv

close :: MonadIO m => ClientT m ()
close = stopNodeT

ping :: (MonadIO m, MonadMask m) => ClientT m Bool
ping = withAgentT $ do
  send Ping
  ret <- receive
  case ret of
    Left _     -> return False
    Right Pong -> return True
    Right _    -> return False

submitJob_ :: (MonadIO m, MonadMask m) => Job -> ClientT m Bool
submitJob_ j = withAgentT $ do
  send (SubmitJob j)
  isSuccess

submitJob
  :: (MonadIO m, MonadMask m)
  => FuncName -> JobName -> Maybe Workload -> Maybe Int64 -> ClientT m Bool
submitJob jFuncName jName w later = do
  jSchedAt <- (+fromMaybe 0 later) <$> liftIO getEpochTime
  submitJob_ Job{jWorkload = fromMaybe "" w, jCount = 0, ..}

runJob_ :: (MonadIO m, MonadMask m) => Job -> ClientT m (Either String Workload)
runJob_ j = withAgentT $ do
  send (RunJob j)
  w <- receive
  case w of
    Left e            -> pure $ Left e
    Right (Result w0) -> pure $ Right w0

runJob
  :: (MonadIO m, MonadMask m)
  => FuncName -> JobName -> Maybe Workload -> Maybe Int64 -> ClientT m (Either String Workload)
runJob jFuncName jName w later = do
  jSchedAt <- (+fromMaybe 0 later) <$> liftIO getEpochTime
  runJob_ Job{jWorkload = fromMaybe "" w, jCount = 0, ..}

dropFunc
  :: (MonadIO m, MonadMask m)
  => FuncName -> ClientT m Bool
dropFunc func = withAgentT $ do
  send (DropFunc func)
  isSuccess

removeJob_
  :: (MonadIO m, MonadMask m)
  => Job -> ClientT m Bool
removeJob_ j = withAgentT $ do
  send (RemoveJob j)
  isSuccess

removeJob
  :: (MonadIO m, MonadMask m)
  => FuncName -> JobName -> ClientT m Bool
removeJob f n = removeJob_ $ newJob f n

isSuccess :: MonadIO m => AgentT m Bool
isSuccess = do
  ret <- receive
  case ret of
    Left _        -> return False
    Right Success -> return True
    Right _       -> return False

status
  :: (MonadIO m, MonadMask m)
  => ClientT m [[ByteString]]
status = withAgentT $ do
  send Status
  ret <- receive_
  return . map (B.split ',') $ B.lines ret

configGet
  :: (MonadIO m, MonadMask m)
  => String -> ClientT m Int
configGet k = withAgentT $ do
  send (ConfigGet (ConfigKey k))
  ret <- receive
  case ret of
    Left _             -> return 0
    Right (Config _ v) -> return v
    Right _            -> return 0

configSet
  :: (MonadIO m, MonadMask m)
  => String -> Int -> ClientT m Bool
configSet k v = withAgentT $ do
  send (ConfigSet (ConfigKey k) v)
  isSuccess

load :: (MonadIO m, MonadMask m) => [Job] -> ClientT m Bool
load jobs = withAgentT $ do
  send (Load jobs)
  isSuccess

dump :: (MonadIO m, MonadMask m) => ClientT m [Job]
dump = withAgentT $ do
  send Dump
  ret <- receive
  case ret of
    Left _            -> return []
    Right (JobList v) -> return v
    Right _           -> return []

shutdown
  :: (MonadIO m, MonadMask m)
  => ClientT m ()
shutdown = withAgentT $ send Shutdown

checkHealth
  :: (MonadIO m, MonadMask m, MonadBaseControl IO m)
  => ClientT m ()
checkHealth = do
  ret <- timeout 10000000 ping
  case ret of
    Nothing -> close
    Just r  -> unless r close
