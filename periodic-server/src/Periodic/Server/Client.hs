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

import           Periodic.Agent               (AgentT, liftAgentT, receive,
                                               send, send_)
import           Periodic.Server.Scheduler
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

type ClientT m = PeriodicT (SchedT m) (TVar Int64)

data ClientEnv m = ClientEnv
  { periodicEnv   :: Env (SchedT m) (TVar Int64)
  , periodicState :: PeriodicState
  , schedState    :: SchedState m
  , schedConfig   :: SchedConfig
  }

runClientT :: Monad m => ClientEnv m -> ClientT m a -> m a
runClientT ClientEnv {..} =
  runSchedT schedState schedConfig . runPeriodicT periodicState periodicEnv

initClientEnv
  :: (MonadIO m, MonadBaseControl IO m)
  => Conn.ConnectionState
  -> Conn.ConnectionConfig
  -> SchedState m
  -> SchedConfig
  -> m (ClientEnv m)
initClientEnv connectionState connectionConfig schedState schedConfig = do
  lastVist <- liftIO $ newTVarIO =<< getEpochTime
  let periodicEnv = initEnv_ lastVist connectionConfig $ handleAgentT lastVist
  periodicState <- liftIO $ initPeriodicState connectionState
  return ClientEnv{..}

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

handleAgentT :: (MonadIO m, MonadBaseControl IO m) => TVar Int64 -> AgentT (SchedT m)  ()
handleAgentT lastVist = do
  liftIO $ do
    t <- getEpochTime
    atomically $ writeTVar lastVist t

  cmd <- receive
  case cmd of
    Left _         -> lift . lift $ Conn.close -- close client
    Right (SubmitJob job) -> do
      liftAgentT $ pushJob job
      send Success
    Right Status -> do
      stats <- liftAgentT $ map toBytes <$> status
      send_ $ B.intercalate "\n" stats
    Right Ping -> send Pong
    Right (DropFunc fn) -> do
      liftAgentT $ dropFunc fn
      send Success
    Right (RemoveJob job) -> do
      liftAgentT $ removeJob job
      send Success
    Right Shutdown -> liftAgentT shutdown
