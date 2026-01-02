{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server
  ( startServer
  ) where


import           Control.Monad                (unless, void, when)
import           Control.Monad.Trans.Class    (lift)
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
import           Metro.Node                   (NodeEnv1 (..), env, runNodeT1,
                                               stopNodeT, withSessionT_)
import           Metro.Server                 (getKeepalive, getMaxPoolSize,
                                               getNodeEnvList, initServerEnv,
                                               runServerT,
                                               setDefaultSessionTimeout,
                                               setKeepalive, setNodeMode,
                                               setOnCheckNodeState,
                                               setOnExcClose, setOnNodeLeave,
                                               setServerName, setSessionMode,
                                               stopServerT)
import qualified Metro.Server                 as M (ServerEnv, startServer)
import qualified Metro.Session                as S (receive, send)
import           Metro.Utils                  (foreverExit, getEpochTime)
import           Periodic.Node                (sessionGen)
import           Periodic.Server.Client       (handleSessionT)
import           Periodic.Server.GrabQueue    (dropAgentList, newGrabQueue,
                                               pushAgent)
import           Periodic.Server.Hook         (Hook)
import           Periodic.Server.Persist      (Persist, PersistConfig)
import           Periodic.Server.Scheduler    (failJob, initSchedEnv,
                                               removeFunc, runSchedT, shutdown,
                                               startSchedT)
import           Periodic.Server.Types        (ClientConfig (..), Command (WC),
                                               ServerCommand (Data))
import           Periodic.Types               (ClientType, Job, Msgid (..),
                                               Nid (..), Packet, getClientType,
                                               getHandle, getResult, getTimeout,
                                               packetRES, regPacketRES)
import           Periodic.Types.ServerCommand (ServerCommand (JobAssign))
import           Periodic.Types.WorkerCommand (isJobAssigned)
import           System.Entropy               (getEntropy)
import           System.Timeout               (timeout)
import           UnliftIO                     (MonadUnliftIO, atomically,
                                               newTVarIO, readTVarIO, tryAny,
                                               writeTVar)

type ServerEnv serv =
  M.ServerEnv serv ClientConfig Nid Msgid (Packet Command)

isJobAssignedCmd :: Command -> Bool
isJobAssignedCmd (WC cmd) = isJobAssigned cmd
isJobAssignedCmd _        = False

doAssignJob :: Transport tp => ServerEnv serv tp -> Nid -> Msgid -> Job -> IO Bool
doAssignJob sEnv nid msgid job = do
  menv0 <- IOMap.lookup nid $ getNodeEnvList sEnv
  case menv0 of
    Nothing   -> return False
    Just env0 -> do
      assigned <- newTVarIO False
      void $ timeout soutS $ tryAny $ runNodeT1 env0 $ withSessionT_ (pure msgid) Nothing $ do
        S.send (packetRES (JobAssign job))
        foreverExit $ \exit -> do
          ret <- lift $ getResult False isJobAssignedCmd <$> S.receive
          atomically $ writeTVar assigned ret
          when ret $ exit ()

      r <- readTVarIO assigned
      when r $ do
        env1 <- runNodeT1 env0 env
        expiredAt <- (+tout) <$> getEpochTime
        IOMap.insert jh expiredAt (wJobQueue env1)

      unless r $ runNodeT1 env0 stopNodeT

      return r

  where jh = getHandle job
        tout = fromIntegral $ getTimeout job
        soutS = 5 * 1000000

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
  -> Hook db
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

  setDefaultSessionTimeout sEnv 60
  setKeepalive sEnv 120

  schedEnv <- initSchedEnv dbconfig grabQueue
              (runServerT sEnv stopServerT)
              (doAssignJob sEnv) (doPushData sEnv) hook (getKeepalive sEnv) (getMaxPoolSize sEnv)

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
