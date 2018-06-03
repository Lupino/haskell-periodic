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

import           Control.Concurrent.STM.TVar
import           Control.Monad.Catch          (MonadCatch)
import           Control.Monad.Haskey         (MonadHaskey)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.STM            (atomically)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Data.Binary                  (encode)
import           Data.Byteable                (toBytes)
import qualified Data.ByteString.Char8        as B (intercalate)
import           Data.ByteString.Lazy         (toStrict)
import           Data.Int                     (Int64)
import           Periodic.Agent               (AgentT, liftC, receive, send,
                                               send_)
import           Periodic.Connection          (ConnEnv, runConnectionT)
import qualified Periodic.Connection          as Conn
import           Periodic.Node                hiding (liftC)
import           Periodic.Server.Scheduler
import           Periodic.Types.ClientCommand
import           Periodic.Types.Internal      (ConfigKey (..))
import           Periodic.Types.ServerCommand
import           Periodic.Utils               (getEpochTime)

type ClientT m = NodeT (TVar Int64) (SchedT m)

data ClientEnv = ClientEnv
  { nodeEnv  :: NodeEnv (TVar Int64)
  , schedEnv :: SchedEnv
  , connEnv  :: ConnEnv
  }

runClientT :: Monad m => ClientEnv -> ClientT m a -> m a
runClientT ClientEnv {..} =
  runSchedT schedEnv
    . runConnectionT connEnv
    . runNodeT nodeEnv

initClientEnv
  :: MonadIO m
  => ConnEnv
  -> SchedEnv
  -> m ClientEnv
initClientEnv connEnv schedEnv = do
  lastVist <- liftIO $ newTVarIO =<< getEpochTime
  nodeEnv <- liftIO $ initEnv lastVist
  return ClientEnv{..}

startClientT
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m, MonadHaskey Schema m)
  => ClientEnv -> m ()
startClientT env0 = runClientT env0 $
  startMainLoop_ . handleAgentT =<< env

close :: MonadIO m => ClientT m ()
close = stopNodeT

getLastVist :: MonadIO m => ClientT m Int64
getLastVist = do
  ref <- env
  liftIO $ readTVarIO ref

handleAgentT :: (MonadIO m, MonadBaseControl IO m, MonadHaskey Schema m) => TVar Int64 -> AgentT (SchedT m)  ()
handleAgentT lastVist = do
  liftIO $ do
    t <- getEpochTime
    atomically $ writeTVar lastVist t

  cmd <- receive
  case cmd of
    Left _         -> liftC Conn.close -- close client
    Right (SubmitJob job) -> do
      lift $ pushJob job
      send Success
    Right (RunJob job) -> do
      lift $ pushJob job
      state <- liftC Conn.statusTVar
      w <- lift $ waitResult state job
      send w
    Right Status -> do
      stats <- lift $ map toBytes <$> status
      send_ $ B.intercalate "\n" stats
    Right Ping -> send Pong
    Right (DropFunc fn) -> do
      lift $ dropFunc fn
      send Success
    Right (RemoveJob job) -> do
      lift $ removeJob job
      send Success
    Right Shutdown -> lift shutdown

    Right (ConfigGet (ConfigKey key)) -> do
      v <- lift $ getConfigInt key
      send $ Config (ConfigKey key) v

    Right (ConfigSet (ConfigKey k) v) -> do
      lift $ setConfigInt k v
      send Success

    Right Dump -> send_ =<< lift (toStrict . encode <$> dumpJob)

    Right (Load jobs) -> do
      lift $ mapM_ pushJob jobs
      send Success
