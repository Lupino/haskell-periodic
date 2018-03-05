{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Periodic.Server.Client
  (
    ClientT
  , ClientEnv
  , initClientEnv
  , close
  , startClientT
  , runClientT
  , getLastVist
  ) where

import           Data.Byteable                (toBytes)
import qualified Data.ByteString.Char8        as B (intercalate)
import qualified Periodic.Connection          as Conn

import           Periodic.Agent               (AgentT, receive, send, send_)
import           Periodic.Server.Scheduler    (Scheduler, dropFunc, pushJob,
                                               removeJob, shutdown, status)
import           Periodic.Types.ClientCommand
import           Periodic.Types.ServerCommand

import           Periodic.Monad
import           Periodic.Utils               (getEpochTime)

import           Control.Concurrent.STM.TVar
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.STM            (atomically)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Data.Int                     (Int64)

type ClientT m = PeriodicT m (TVar Int64)

data ClientEnv m = ClientEnv
  { periodicEnv   :: Env m (TVar Int64)
  , periodicState :: PeriodicState
  }

runClientT :: Monad m => ClientEnv m -> ClientT m a -> m a
runClientT ClientEnv {..} = runPeriodicT periodicState periodicEnv

initClientEnv
  :: (MonadIO m)
  => Conn.ConnectionState
  -> Conn.ConnectionConfig
  -> Scheduler
  -> m (ClientEnv m)
initClientEnv connectionState connectionConfig sched = do
  lastVist <- liftIO $ newTVarIO =<< getEpochTime
  let env0 = initEnv_ lastVist connectionConfig $ handleAgentT sched lastVist
  state0 <- liftIO $ initPeriodicState connectionState
  return $ ClientEnv env0 state0

startClientT
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => ClientEnv m -> m ()
startClientT env0 = runClientT env0 startMainLoop

close :: MonadIO m => ClientT m ()
close = stopPeriodicT

getLastVist :: MonadIO m => ClientT m Int64
getLastVist = do
  ref <- env
  liftIO $ readTVarIO ref

handleAgentT :: MonadIO m => Scheduler -> TVar Int64 -> AgentT m  ()
handleAgentT sched lastVist = do
  liftIO $ do
    t <- getEpochTime
    atomically $ writeTVar lastVist t

  cmd <- receive
  case cmd of
    Left _     -> lift . lift $ Conn.close -- close client
    Right (SubmitJob job) -> do
      liftIO $ pushJob sched job
      send Success
    Right Status -> do
      stats <- liftIO $ map toBytes <$> status sched
      send_ $ B.intercalate "\n" stats
    Right Ping -> send Pong
    Right (DropFunc fn) -> do
      liftIO $ dropFunc sched fn
      send Success
    Right (RemoveJob job) -> do
      liftIO $ removeJob sched job
      send Success
    Right Shutdown -> liftIO $ shutdown sched
