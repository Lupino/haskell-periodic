{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Periodic.Server.Client
  ( ClientT
  , handleSessionT
  ) where


import           Control.Monad                (unless, when)
import           Control.Monad.Trans.Class    (lift)
import           Data.Binary                  (encode)
import           Data.Byteable                (toBytes)
import qualified Data.ByteString.Char8        as B (intercalate)
import           Data.ByteString.Lazy         (toStrict)
import qualified Data.IOMap                   as IOMap
import           Data.List                    (delete)
import           Metro.Class                  (Transport)
import           Metro.Conn                   (fromConn)
import qualified Metro.Conn                   as Conn
import           Metro.Session                (env, getSessionEnv1, ident,
                                               receive, send)
import           Periodic.Node
import           Periodic.Server.Persist      (Persist)
import           Periodic.Server.Scheduler
import           Periodic.Server.Types
import qualified Periodic.Types.ClientCommand as CC
import           Periodic.Types.Internal      (ConfigKey (..))
import           Periodic.Types.Job           (getFuncName, initJob)
import           Periodic.Types.Packet        (getPacketData, packetRES)
import qualified Periodic.Types.WorkerCommand as WC
import           System.Log.Logger            (errorM)
import           UnliftIO


type ClientT db tp m = NodeT ClientConfig Command tp (SchedT db m)

handleClientSessionT
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => CC.ClientCommand -> SessionT ClientConfig Command tp (SchedT db m) ()
handleClientSessionT (CC.SubmitJob job) = do
  lift $ pushJob job
  send $ packetRES Success
handleClientSessionT (CC.RunJob job) = do
  c <- lift . canRun $ getFuncName job
  if c then do
    (!nid, !msgid) <- ident <$> getSessionEnv1
    lift $ prepareWait job nid msgid False
    lift $ pushJob job
  else send $ packetRES NoWorker

handleClientSessionT (CC.RecvData job) = do
  c <- lift . canRun $ getFuncName job
  if c then do
    (!nid, !msgid) <- ident <$> getSessionEnv1
    lift $ prepareWait job nid msgid True
    send $ packetRES Success
  else send $ packetRES NoWorker

handleClientSessionT CC.Status = do
  stats <- lift $ map toBytes <$> status
  send . packetRES . Data $ B.intercalate "\n" stats

handleClientSessionT CC.Ping = send $ packetRES Pong

handleClientSessionT (CC.DropFunc fn) = do
  lift $ dropFunc fn
  send $ packetRES Success

handleClientSessionT (CC.RemoveJob fn jn) = do
  lift $ removeJob $ initJob fn jn
  send $ packetRES Success
handleClientSessionT CC.Shutdown = lift shutdown

handleClientSessionT (CC.ConfigGet (ConfigKey key)) = do
  v <- lift $ getConfigInt key
  send $ packetRES $ Config v

handleClientSessionT (CC.ConfigSet (ConfigKey key) v) = do
  lift $ setConfigInt key v
  send $ packetRES Success

handleClientSessionT CC.Dump = send =<< lift (packetRES . Data . toStrict . encode <$> dumpJob)

handleClientSessionT (CC.Load jobs) = do
  lift $ mapM_ pushJob jobs
  send $ packetRES Success

handleWorkerSessionT
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => ClientConfig -> WC.WorkerCommand -> SessionT ClientConfig Command tp (SchedT db m) ()
handleWorkerSessionT ClientConfig {..} WC.GrabJob = do
  (_, !msgid) <- ident <$> getSessionEnv1
  atomically $ do
    msgidList <- readTVar wMsgidList
    if msgid `elem` msgidList
       then pure ()
       else writeTVar wMsgidList $! msgid : msgidList

handleWorkerSessionT ClientConfig {..} (WC.WorkDone jh w) = do
  IOMap.delete jh wJobQueue
  lift $ doneJob jh w
  send $ packetRES Success
handleWorkerSessionT ClientConfig {..} (WC.WorkData jh w) = do
  lift $ workData jh w
  send $ packetRES Success
handleWorkerSessionT ClientConfig {..} (WC.WorkFail jh) = do
  IOMap.delete jh wJobQueue
  lift $ failJob jh
  send $ packetRES Success
handleWorkerSessionT ClientConfig {..} (WC.SchedLater jh l s) = do
  IOMap.delete jh wJobQueue
  lift $ schedLaterJob jh l s
  send $ packetRES Success
handleWorkerSessionT ClientConfig {..} WC.Sleep = send $ packetRES Noop
handleWorkerSessionT ClientConfig {..} WC.Ping = send $ packetRES Pong
handleWorkerSessionT ClientConfig {..} (WC.CanDo fn) = do
  has <- atomically $ do
    funcList <- readTVar wFuncList
    if fn `elem` funcList
       then pure True
       else do
         writeTVar wFuncList $! fn : funcList
         pure False
  unless has $ lift $ addFunc fn
  send $ packetRES Success
handleWorkerSessionT ClientConfig {..} (WC.CantDo fn) = do
  has <- atomically $ do
    funcList <- readTVar wFuncList
    if fn `elem` funcList
       then do
         writeTVar wFuncList $! delete fn funcList
         pure True
       else pure False
  when has $ lift $ removeFunc fn
  send $ packetRES Success
handleWorkerSessionT ClientConfig {..} (WC.Broadcast fn) = do
  has <- atomically $ do
    funcList <- readTVar wFuncList
    if fn `elem` funcList
       then pure True
       else do
         writeTVar wFuncList $! fn : funcList
         pure False
  unless has $ lift $ broadcastFunc fn True
  send $ packetRES Success
handleWorkerSessionT _ (WC.Acquire n c jh) = do
  r <- lift $ acquireLock n c jh
  send $ packetRES $ Acquired r
handleWorkerSessionT _ (WC.Release n jh) = do
  lift $ releaseLock n jh
  send $ packetRES Success

handleSessionT
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => SessionT ClientConfig Command tp (SchedT db m) ()
handleSessionT = do
  mcmd <- receive
  case mcmd of
    Nothing -> do
      liftIO $ errorM "Periodic.Server.Client" "Client error"
      fromConn Conn.close -- close client
    Just pkt ->
      case getPacketData pkt of
        SC _   -> pure ()
        CC cmd -> handleClientSessionT cmd
        WC cmd -> do
          env0 <- env
          handleWorkerSessionT env0 cmd
