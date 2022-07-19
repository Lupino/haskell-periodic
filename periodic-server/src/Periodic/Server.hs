{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server
  ( startServer
  ) where


import           Control.Monad                (void)
import           Data.Binary.Get              (getWord32be, runGet)
import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (fromStrict)
import qualified Data.IOMap                   as IOMap
import           Data.Map.Strict              (filterWithKey)
import           Metro                        (NodeMode (..), SessionMode (..))
import           Metro.Class                  (Servable (STP),
                                               Transport (TransportConfig),
                                               setPacketId)
import qualified Metro.Class                  as S (Servable (ServerConfig))
import           Metro.Conn                   (receive_, runConnT, send)
import           Metro.Node                   (NodeEnv1 (..), env, runNodeT1)
import           Metro.Server                 (getMaxPoolSize, getNodeEnvList,
                                               initServerEnv, runServerT,
                                               setDefaultSessionTimeout,
                                               setKeepalive, setNodeMode,
                                               setOnCheckNodeState,
                                               setOnExcClose, setOnNodeLeave,
                                               setServerName, setSessionMode,
                                               stopServerT)
import qualified Metro.Server                 as M (ServerEnv, startServer)
import           Metro.Utils                  (getEpochTime)
import           Periodic.Node                (sessionGen)
import           Periodic.Server.Client       (handleSessionT)
import           Periodic.Server.GrabQueue    (dropAgentList, newGrabQueue,
                                               pushAgent)
import           Periodic.Server.Hook         (Hook)
import           Periodic.Server.Persist      (Persist, PersistConfig)
import           Periodic.Server.Scheduler    (failJob, initSchedEnv,
                                               removeFunc, runSchedT, shutdown,
                                               startSchedT)
import           Periodic.Server.Types        (ClientConfig (..), Command,
                                               ServerCommand (Data))
import           Periodic.Types               (ClientType, Job, Msgid (..),
                                               Nid (..), Packet, getClientType,
                                               getHandle, getTimeout, packetRES,
                                               regPacketRES)
import           Periodic.Types.ServerCommand (ServerCommand (JobAssign))
import           System.Entropy               (getEntropy)

import           UnliftIO                     (MonadUnliftIO, newTVarIO,
                                               readTVarIO, tryAny)

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
          expiredAt <- (+tout) <$> getEpochTime
          IOMap.insert jh expiredAt (wJobQueue env1)
          return True

  where jh = getHandle job
        tout = fromIntegral $ getTimeout job

doPushData :: Transport tp => ServerEnv serv tp -> Nid -> Msgid -> ByteString -> IO ()
doPushData sEnv nid msgid w = do
  menv0 <- IOMap.lookup nid $ getNodeEnvList sEnv
  case menv0 of
    Nothing   -> return ()
    Just env0 -> do
      void
        $ tryAny
        $ runConnT (connEnv env0)
        $ send
        $ setPacketId msgid
        $ packetRES (Data w)

startServer
  :: (Servable serv, Transport tp, Persist db, MonadUnliftIO m)
  => PersistConfig db
  -> (TransportConfig (STP serv) -> TransportConfig tp)
  -> S.ServerConfig serv
  -> Hook
  -> Int
  -> Int
  -> m ()
startServer dbconfig mk config hook pushTaskSize schedTaskSize = do
  grabQueue <- newGrabQueue
  sEnv <- fmap mapEnv . initServerEnv config sessionGen mk $ \_ _ connEnv0 -> do
    (_ :: ClientType) <- getClientType <$> runConnT connEnv0 receive_
    nid <- getEntropy 4
    runConnT connEnv0 $ send (regPacketRES $ Data nid)
    let nidV = Nid $! runGet getWord32be $ fromStrict nid
    wFuncList  <- newTVarIO []
    wJobQueue  <- IOMap.empty
    wMsgidList <- newTVarIO []
    pushAgent grabQueue wFuncList nidV wMsgidList
    return $ Just (nidV, ClientConfig {..})

  setDefaultSessionTimeout sEnv 100
  setKeepalive sEnv 500

  schedEnv <- initSchedEnv dbconfig grabQueue
              (runServerT sEnv stopServerT)
              (doAssignJob sEnv) (doPushData sEnv) hook (getMaxPoolSize sEnv)

  setOnNodeLeave sEnv $ \nid ClientConfig {..} ->
    runSchedT schedEnv $ do
      mapM_ failJob =<< IOMap.keys wJobQueue
      mapM_ removeFunc =<< readTVarIO wFuncList
      dropAgentList grabQueue nid

  setOnCheckNodeState sEnv $ \_ ClientConfig {..} -> do
    now <- getEpochTime
    handles <- map fst . filter (\(_, t) -> t < now) <$> IOMap.toList wJobQueue
    IOMap.modifyIOMap (filterWithKey (\_ expiredAt -> expiredAt > now)) wJobQueue
    runSchedT schedEnv $ mapM_ failJob handles

  runSchedT schedEnv $ do
    startSchedT pushTaskSize schedTaskSize
    M.startServer sEnv handleSessionT
    shutdown
  where mapEnv :: ServerEnv serv tp -> ServerEnv serv tp
        mapEnv =
          setNodeMode Multi
          . setSessionMode SingleAction
          . setServerName "Periodic"
          . setOnExcClose True
