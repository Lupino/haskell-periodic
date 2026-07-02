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


import           Control.Monad                (filterM, unless, when)
import           Control.Monad.Trans.Class    (lift)
import           Data.Binary                  (encode)
import           Data.Byteable                (toBytes)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B (intercalate)
import           Data.ByteString.Lazy         (toStrict)
import qualified Data.IOMap                   as IOMap
import           Data.List                    (find)
import qualified Data.Set                     as Set
import           Metro.Class                  (Transport)
import           Metro.Conn                   (fromConn)
import qualified Metro.Conn                   as Conn
import           Metro.Session                (env, getSessionEnv1, ident,
                                               receive, send)
import           Periodic.Node
import           Periodic.Server.Auth         (isAdminAllowed, isFuncAllowed,
                                               isRoleAllowed)
import           Periodic.Server.FuncStat     (FuncStat (..))
import           Periodic.Server.Persist      (Persist)
import           Periodic.Server.Scheduler
import           Periodic.Server.Types
import qualified Periodic.Types.ClientCommand as CC
import           Periodic.Types.Internal      (ConfigKey (..))
import           Periodic.Types.Job           (FuncName, getFuncName, initJob)
import           Periodic.Types.Packet        (getPacketData, packetRES)
import qualified Periodic.Types.WorkerCommand as WC
import           System.Log.Logger            (errorM)
import           UnliftIO


type ClientT db tp m = NodeT ClientConfig Command tp (SchedT db m)

handleClientSessionT
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => CC.ClientCommand -> SessionT ClientConfig Command tp (SchedT db m) ()
handleClientSessionT (CC.SubmitJob job) = do
  allowed <- checkFunc "SubmitJob" $ getFuncName job
  when allowed $ do
    lift $ pushJob job
    send $ packetRES Success
handleClientSessionT (CC.RunJob job) = do
  allowed <- checkFunc "RunJob" $ getFuncName job
  when allowed $ do
    c <- lift . canRun $ getFuncName job
    if c then do
      (!nid, !msgid) <- ident <$> getSessionEnv1
      lift $ prepareWait job nid msgid False
      lift $ runJobNow job
    else send $ packetRES NoWorker

handleClientSessionT (CC.RecvData job) = do
  allowed <- checkFunc "RecvData" $ getFuncName job
  when allowed $ do
    c <- lift . canRun $ getFuncName job
    if c then do
      (!nid, !msgid) <- ident <$> getSessionEnv1
      lift $ prepareWait job nid msgid True
      send $ packetRES Success
    else send $ packetRES NoWorker

handleClientSessionT CC.Status = do
  cfg <- env
  stats <- filterStatus cfg =<< lift status
  send . packetRES . Data $ B.intercalate "\n" stats

handleClientSessionT CC.Ping = send $ packetRES Pong

handleClientSessionT (CC.DropFunc fn) = do
  allowed <- checkFunc "DropFunc" fn
  when allowed $ do
    lift $ dropFunc fn
    send $ packetRES Success

handleClientSessionT (CC.RemoveJob fn jn) = do
  allowed <- checkFunc "RemoveJob" fn
  when allowed $ do
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
  allowed <- checkFuncs "Load" $ map getFuncName jobs
  when allowed $ do
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

  send $ packetRES Success

handleWorkerSessionT ClientConfig {..} (WC.WorkDone jh w) = do
  IOMap.delete jh wJobQueue
  lift $ doneJob jh w
  send $ packetRES Success
handleWorkerSessionT _ (WC.WorkData jh w) = do
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
handleWorkerSessionT _ WC.Sleep = send $ packetRES Noop
handleWorkerSessionT _ WC.Ping = send $ packetRES Pong
handleWorkerSessionT ClientConfig {..} (WC.CanDo fn) = do
  allowed <- checkFunc "CanDo" fn
  when allowed $ do
    has <- atomically $ do
      funcList <- readTVar wFuncList
      if fn `Set.member` funcList
         then pure True
         else do
           writeTVar wFuncList $! Set.insert fn funcList
           pure False
    unless has $ lift $ addFunc fn
    send $ packetRES Success
handleWorkerSessionT ClientConfig {..} (WC.CantDo fn) = do
  has <- atomically $ do
    funcList <- readTVar wFuncList
    if fn `Set.member` funcList
       then do
         writeTVar wFuncList $! Set.delete fn funcList
         pure True
       else pure False
  when has $ lift $ removeFunc fn
  send $ packetRES Success
handleWorkerSessionT ClientConfig {..} (WC.Broadcast fn) = do
  allowed <- checkFunc "Broadcast" fn
  when allowed $ do
    has <- atomically $ do
      funcList <- readTVar wFuncList
      if fn `Set.member` funcList
         then pure True
         else do
           writeTVar wFuncList $! Set.insert fn funcList
           pure False
    unless has $ lift $ broadcastFunc fn True
    send $ packetRES Success
handleWorkerSessionT _ (WC.Acquire n c jh) = do
  r <- lift $ acquireLock n c jh
  send $ packetRES $ Acquired r
handleWorkerSessionT _ (WC.Release n jh) = do
  lift $ releaseLock n jh
  send $ packetRES Success

handleWorkerSessionT _ WC.JobAssigned = pure ()
handleWorkerSessionT _ WC.JobUnassigned = pure ()

filterStatus
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => ClientConfig -> [FuncStat] -> SessionT ClientConfig Command tp (SchedT db m) [ByteString]
filterStatus ClientConfig {wAuth = Nothing} stats = pure $ map toBytes stats
filterStatus ClientConfig {wAuth = Just auth, wIdentity = ident0} stats =
  map toBytes <$> filterM isAllowed stats
  where
    isAllowed FuncStat {..} = liftIO $ isFuncAllowed auth ident0 sFuncName

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
        SC Unknown -> send $ packetRES Unknown
        SC _       -> pure ()
        CC cmd -> do
          env0 <- env
          allowed <- checkRole env0 "ClientCommand"
          when allowed $ handleClientSessionT cmd
        WC cmd -> do
          env0 <- env
          allowed <- checkRole env0 $ workerCommandName cmd
          when allowed $ handleWorkerSessionT env0 cmd

checkRole
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => ClientConfig -> String -> SessionT ClientConfig Command tp (SchedT db m) Bool
checkRole cfg@ClientConfig {..} action = do
  roleAllowed <- checkAuthRole cfg action
  adminAllowed <- checkAdminRole cfg
  if roleAllowed
     then if adminAllowed
             then pure True
             else checkCommandRole
     else pure False
  where
    checkCommandRole =
      case wRole of
        ClientRoleWorker -> pure True
        ClientRoleClient ->
          if action == "ClientCommand"
             then pure True
             else do
               liftIO $ errorM "Periodic.Server.Client" $
                 "Rejected client-only connection command " ++ action ++
                 " for " ++ show wIdentity
               send $ packetRES NoWorker
               pure False

checkAuthRole
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => ClientConfig -> String -> SessionT ClientConfig Command tp (SchedT db m) Bool
checkAuthRole ClientConfig {..} action =
  case wAuth of
    Nothing -> pure True
    Just auth -> do
      allowed <- liftIO $ isRoleAllowed auth wIdentity $ clientRoleName wRole
      unless allowed $ do
        liftIO $ errorM "Periodic.Server.Client" $
          "Rejected unauthorized role for " ++ show wIdentity ++
          " role=" ++ show (clientRoleName wRole) ++
          " action=" ++ action
        send $ packetRES NoWorker
      pure allowed

checkAdminRole
  :: MonadUnliftIO m
  => ClientConfig -> SessionT ClientConfig Command tp (SchedT db m) Bool
checkAdminRole ClientConfig {..} =
  case wAuth of
    Nothing -> pure False
    Just auth -> liftIO $ isAdminAllowed auth wIdentity

clientRoleName :: ClientRole -> ByteString
clientRoleName ClientRoleClient = "client"
clientRoleName ClientRoleWorker = "worker"

workerCommandName :: WC.WorkerCommand -> String
workerCommandName WC.GrabJob         = "GrabJob"
workerCommandName (WC.WorkDone _ _)  = "WorkDone"
workerCommandName (WC.WorkData _ _)  = "WorkData"
workerCommandName (WC.WorkFail _)    = "WorkFail"
workerCommandName (WC.SchedLater _ _ _) = "SchedLater"
workerCommandName WC.Sleep           = "Sleep"
workerCommandName WC.Ping            = "WorkerPing"
workerCommandName (WC.CanDo _)       = "CanDo"
workerCommandName (WC.CantDo _)      = "CantDo"
workerCommandName (WC.Broadcast _)   = "Broadcast"
workerCommandName (WC.Acquire _ _ _) = "Acquire"
workerCommandName (WC.Release _ _)   = "Release"
workerCommandName WC.JobAssigned     = "JobAssigned"
workerCommandName WC.JobUnassigned   = "JobUnassigned"

checkFunc
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => String -> FuncName -> SessionT ClientConfig Command tp (SchedT db m) Bool
checkFunc action fn = do
  cfg <- env
  allowed <- case wAuth cfg of
    Nothing   -> pure True
    Just auth -> liftIO $ isFuncAllowed auth (wIdentity cfg) fn
  unless allowed $ do
    liftIO $ errorM "Periodic.Server.Client" $
      "Rejected unauthorized " ++ action ++ " for " ++ show (wIdentity cfg) ++ " func=" ++ show fn
    send $ packetRES NoWorker
  pure allowed

checkFuncs
  :: (MonadUnliftIO m, Persist db, Transport tp)
  => String -> [FuncName] -> SessionT ClientConfig Command tp (SchedT db m) Bool
checkFuncs action funcs = do
  cfg <- env
  case wAuth cfg of
    Nothing -> pure True
    Just auth -> do
      allowed <- liftIO $ mapM (isFuncAllowed auth (wIdentity cfg)) funcs
      case find (not . snd) $ zip funcs allowed of
        Nothing -> pure True
        Just (fn, _) -> do
          liftIO $ errorM "Periodic.Server.Client" $
            "Rejected unauthorized " ++ action ++ " for " ++ show (wIdentity cfg) ++ " func=" ++ show fn
          send $ packetRES NoWorker
          pure False
