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
import qualified Data.ByteString.Char8        as B (intercalate)
import           Data.ByteString.Lazy         (toStrict)
import           Data.Byteable                (toBytes)
import qualified Data.IOMap                   as IOMap
import           Metro.Class                  (Transport)
import           Metro.Conn                   (fromConn)
import qualified Metro.Conn                   as Conn
import           Metro.Session                (env, getSessionEnv1, receive,
                                               send)
import           Periodic.Node
import           Periodic.Server.Persist      (Persist)
import           Periodic.Server.Scheduler
import           Periodic.Server.Types
import qualified Periodic.Types.ClientCommand as CC
import           Periodic.Types.Internal      (ConfigKey (..))
import           Periodic.Types.Job           (getFuncName, initJob)
import           Periodic.Types.Packet        (getPacketData, packetRES)
import qualified Periodic.Types.WorkerCommand as WC
import           Prelude                      hiding (elem)
import           System.Log.Logger            (errorM)
import           UnliftIO


type ClientT db tp m = NodeT ClientConfig Command tp (SchedT db tp m)

handleClientSessionT
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => CC.ClientCommand -> SessionT ClientConfig Command tp (SchedT db tp m) ()
handleClientSessionT (CC.SubmitJob job) = do
  lift $ pushJob job
  send $ packetRES Success
handleClientSessionT (CC.RunJob job) = do
  preR <- lift $ lookupPrevResult job
  case preR of
    Just v -> send $ packetRES $ Data v
    Nothing -> do
      c <- lift . canRun $ getFuncName job
      if c then do
        lift $ prepareWait job
        lift $ pushJob job
        state <- fromConn Conn.statusTVar
        w <- lift $ waitResult state job
        send . packetRES $ Data w
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
  => ClientConfig -> WC.WorkerCommand -> SessionT ClientConfig Command tp (SchedT db tp m) ()
handleWorkerSessionT ClientConfig {..} WC.GrabJob = do
  env0 <- getSessionEnv1
  lift $ pushGrab wFuncList wJobQueue env0
handleWorkerSessionT ClientConfig {..} (WC.WorkDone jh w) = do
  lift $ doneJob jh w
  IOMap.delete jh wJobQueue
handleWorkerSessionT ClientConfig {..} (WC.WorkFail jh) = do
  lift $ failJob jh
  IOMap.delete jh wJobQueue
handleWorkerSessionT ClientConfig {..} (WC.SchedLater jh l s) = do
  lift $ schedLaterJob jh l s
  IOMap.delete jh wJobQueue
handleWorkerSessionT ClientConfig {..} WC.Sleep = send $ packetRES Noop
handleWorkerSessionT ClientConfig {..} WC.Ping = send $ packetRES Pong
handleWorkerSessionT ClientConfig {..} (WC.CanDo fn) = do
  has <- IOMap.member fn wFuncList
  unless has $ do
    lift $ addFunc fn
    IOMap.insert fn () wFuncList
handleWorkerSessionT ClientConfig {..} (WC.CantDo fn) = do
  has <- IOMap.member fn wFuncList
  when has $ do
    lift $ removeFunc fn
    IOMap.delete fn wFuncList
handleWorkerSessionT ClientConfig {..} (WC.Broadcast fn) = do
  has <- IOMap.member fn wFuncList
  unless has $ do
    lift $ broadcastFunc fn True
    IOMap.insert fn () wFuncList
handleWorkerSessionT _ (WC.Acquire n c jh) = do
  r <- lift $ acquireLock n c jh
  send $ packetRES $ Acquired r
handleWorkerSessionT _ (WC.Release n jh) = lift $ releaseLock n jh

handleSessionT
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => SessionT ClientConfig Command tp (SchedT db tp m) ()
handleSessionT = do
  mcmd <- receive
  case mcmd of
    Nothing -> do
      liftIO $ errorM "Periodic.Server.Client" "Client error"
      fromConn Conn.close -- close client
    Just pkt ->
      case getPacketData pkt of
        CC cmd -> handleClientSessionT cmd
        WC cmd -> do
          env0 <- env
          handleWorkerSessionT env0 cmd
