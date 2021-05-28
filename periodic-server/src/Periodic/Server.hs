{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server
  ( startServer
  ) where


import           Data.Binary.Get              (getWord32be, runGet)
import           Data.ByteString.Lazy         (fromStrict)
import qualified Data.IOMap                   as IOMap
import qualified Data.IOMap.STM               as IOMapS
import           Metro                        (NodeMode (..), SessionMode (..))
import           Metro.Class                  (Servable (STP),
                                               Transport (TransportConfig),
                                               setPacketId)
import qualified Metro.Class                  as S (Servable (ServerConfig))
import           Metro.Conn                   (receive, runConnT, send)
import           Metro.Node                   (NodeEnv1 (..), env, runNodeT1,
                                               uEnv)
import           Metro.Server                 (getNodeEnvList, initServerEnv,
                                               runServerT,
                                               setDefaultSessionTimeout,
                                               setKeepalive, setNodeMode,
                                               setOnExcClose, setOnNodeLeave,
                                               setServerName, setSessionMode,
                                               stopServerT)
import qualified Metro.Server                 as M (ServerEnv, startServer)
import           Periodic.Node                (sessionGen)
import           Periodic.Server.Client       (handleSessionT)
import           Periodic.Server.GrabQueue
import           Periodic.Server.Persist      (Persist, PersistConfig)
import           Periodic.Server.Scheduler    (failJob, initSchedEnv,
                                               removeFunc, runSchedT, shutdown,
                                               startSchedT)
import           Periodic.Server.Types        (ClientConfig (..), Command,
                                               ServerCommand (Data))
import           Periodic.Types               (ClientType, FuncName, Job,
                                               Msgid (..), Nid (..), Packet,
                                               getClientType, getHandle,
                                               packetRES, regPacketRES)
import           Periodic.Types.ServerCommand (ServerCommand (JobAssign))
import           System.Entropy               (getEntropy)

import           UnliftIO                     (MonadUnliftIO, STM, atomically,
                                               modifyTVar', newTVarIO, readTVar,
                                               readTVarIO, retrySTM, tryAny)

type ServerEnv serv =
  M.ServerEnv serv ClientConfig Nid Msgid (Packet Command)

doAssignJob :: Transport tp => ServerEnv serv tp -> Nid -> Msgid -> Job -> IO Bool
doAssignJob sEnv nid msgid job = do
  menv0 <- IOMap.lookup nid $ getNodeEnvList sEnv
  case menv0 of
    Nothing   -> return False
    Just env0 -> do
      r <- tryAny
        $ runConnT (connEnv env0)
        $ send
        $ setPacketId msgid
        $ packetRES (JobAssign job)
      case r of
        Left _  -> return False
        Right _ -> do
          env1 <- runNodeT1 env0 env
          atomically $ modifyTVar' (wJobQueue env1) (getHandle job :)
          return True

waitAndPopMsgid :: ServerEnv serv tp -> GrabQueue -> FuncName -> IO (Nid, Msgid)
waitAndPopMsgid sEnv gq func = atomically $ do
  nids <- IOMapS.keys nodeList
  ret <- go nids
  case ret of
    Nothing           -> retrySTM
    Just (nid, msgid) -> pure (nid, msgid)
  where go :: [Nid] -> STM (Maybe (Nid, Msgid))
        go [] = pure Nothing
        go (x:xs) = do
          menv0 <- IOMapS.lookup x nodeList
          case menv0 of
            Nothing   -> go xs
            Just env0 -> do
              funcs <- readTVar $ wFuncList . uEnv $ nodeEnv env0
              if func `elem` funcs
                 then do
                   ret <- popAgentSTM gq x
                   case ret of
                     Nothing    -> go xs
                     Just msgid -> pure (Just (x, msgid))
                 else go xs

        nodeList = getNodeEnvList sEnv

startServer
  :: (Servable serv, Transport tp, Persist db, MonadUnliftIO m)
  => PersistConfig db
  -> (TransportConfig (STP serv) -> TransportConfig tp)
  -> S.ServerConfig serv
  -> m ()
startServer dbconfig mk config = do
  sEnv <- fmap mapEnv . initServerEnv config sessionGen mk $ \_ _ connEnv0 -> do
    (_ :: ClientType) <- getClientType <$> runConnT connEnv0 receive
    nid <- getEntropy 4
    runConnT connEnv0 $ send (regPacketRES $ Data nid)
    wFuncList <- newTVarIO []
    wJobQueue <- newTVarIO []
    return $ Just (Nid $! runGet getWord32be $ fromStrict nid, ClientConfig {..})

  setDefaultSessionTimeout sEnv 100
  setKeepalive sEnv 500

  schedEnv <- initSchedEnv dbconfig
              (runServerT sEnv stopServerT)
              (doAssignJob sEnv)
              (waitAndPopMsgid sEnv)

  setOnNodeLeave sEnv $ \_ ClientConfig {..} ->
    runSchedT schedEnv $ do
      mapM_ failJob =<< readTVarIO wJobQueue
      mapM_ removeFunc =<< readTVarIO wFuncList

  runSchedT schedEnv $ do
    startSchedT
    M.startServer sEnv handleSessionT
    shutdown
  where mapEnv :: ServerEnv serv tp -> ServerEnv serv tp
        mapEnv =
          setNodeMode Multi
          . setSessionMode SingleAction
          . setServerName "Periodic"
          . setOnExcClose True
