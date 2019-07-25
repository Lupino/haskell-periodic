{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Periodic.Server.Client
  ( ClientT
  , ClientEnv
  , initClientEnv
  , close
  , startClientT
  , runClientT
  , getLastVist
  ) where

import           Control.Monad.Trans.Class    (lift)
import           Data.Binary                  (encode)
import           Data.Byteable                (toBytes)
import qualified Data.ByteString.Char8        as B (intercalate)
import           Data.ByteString.Lazy         (toStrict)
import           Data.Int                     (Int64)
import           Periodic.Agent               (AgentT, receive, send, send_)
import           Periodic.Connection          (ConnEnv, fromConn,
                                               runConnectionT)
import qualified Periodic.Connection          as Conn
import           Periodic.Node
import           Periodic.Server.Persist      (Persist)
import           Periodic.Server.Scheduler
import           Periodic.Transport           (Transport)
import           Periodic.Types.ClientCommand
import           Periodic.Types.Internal      (ConfigKey (..))
import           Periodic.Types.Job           (getFuncName, initJob)
import           Periodic.Types.ServerCommand
import           Periodic.Utils               (getEpochTime)
import           UnliftIO


type ClientT db tp m = NodeT (TVar Int64) tp (SchedT db tp m)

data ClientEnv db tp = ClientEnv
  { nodeEnv  :: NodeEnv (TVar Int64)
  , schedEnv :: SchedEnv db tp
  , connEnv  :: ConnEnv tp
  }

runClientT :: Monad m => ClientEnv db tp -> ClientT db tp m a -> m a
runClientT ClientEnv {..} =
  runSchedT schedEnv
    . runConnectionT connEnv
    . runNodeT nodeEnv

initClientEnv
  :: MonadIO m
  => ConnEnv tp
  -> SchedEnv db tp
  -> m (ClientEnv db tp)
initClientEnv connEnv schedEnv = do
  lastVist <- newTVarIO =<< getEpochTime
  nodeEnv <- initEnv lastVist
  return ClientEnv{..}

startClientT :: (MonadUnliftIO m, Persist db, Transport tp) => ClientEnv db tp -> m ()
startClientT env0 = runClientT env0 $
  startMainLoop_ . handleAgentT =<< env

close :: (MonadIO m, Transport tp) => ClientT db tp m ()
close = stopNodeT

getLastVist :: MonadIO m => ClientT db tp m Int64
getLastVist = do
  ref <- env
  readTVarIO ref

handleAgentT :: (MonadUnliftIO m, Persist db, Transport tp) => TVar Int64 -> AgentT tp (SchedT db tp m) ()
handleAgentT lastVist = do
  t <- getEpochTime
  atomically $ writeTVar lastVist t

  cmd <- receive
  case cmd of
    Left _         -> fromConn Conn.close -- close client
    Right (SubmitJob job) -> do
      lift $ pushJob job
      send Success
    Right (RunJob job) -> do
      preR <- lift $ lookupPrevResult job
      case preR of
        Just v -> send $ Data v
        Nothing -> do
          c <- lift . canRun $ getFuncName job
          if c then do
            lift $ prepareWait job
            lift $ pushJob job
            state <- fromConn Conn.statusTVar
            w <- lift $ waitResult state job
            send $ Data w
          else send NoWorker

    Right Status -> do
      stats <- lift $ map toBytes <$> status
      send_ $ B.intercalate "\n" stats
    Right Ping -> send Pong
    Right (DropFunc fn) -> do
      lift $ dropFunc fn
      send Success
    Right (RemoveJob fn jn) -> do
      lift $ removeJob $ initJob fn jn
      send Success
    Right Shutdown -> lift shutdown

    Right (ConfigGet (ConfigKey key)) -> do
      v <- lift $ getConfigInt key
      send $ Config v

    Right (ConfigSet (ConfigKey k) v) -> do
      lift $ setConfigInt k v
      send Success

    Right Dump -> send_ =<< lift (toStrict . encode <$> dumpJob)

    Right (Load jobs) -> do
      lift $ mapM_ pushJob jobs
      send Success
