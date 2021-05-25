{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server
  ( startServer
  ) where


import qualified Data.IOMap                as IOMap
import           Metro                     (NodeMode (..), SessionMode (..))
import           Metro.Class               (Servable (STP),
                                            Transport (TransportConfig))
import qualified Metro.Class               as S (Servable (ServerConfig))
import           Metro.Conn                (receive, runConnT, send)
import           Metro.Server              (initServerEnv, runServerT,
                                            setDefaultSessionTimeout,
                                            setKeepalive, setNodeMode,
                                            setOnExcClose, setOnNodeLeave,
                                            setServerName, setSessionMode,
                                            stopServerT)
import qualified Metro.Server              as M (ServerEnv, startServer)
import           Periodic.Node             (sessionGen)
import           Periodic.Server.Client    (handleSessionT)
import           Periodic.Server.Persist   (Persist, PersistConfig)
import           Periodic.Server.Scheduler (failJob, initSchedEnv, removeFunc,
                                            runSchedT, shutdown, startSchedT)
import           Periodic.Server.Types     (ClientConfig (..), Command,
                                            ServerCommand (Data))
import           Periodic.Types            (ClientType, Msgid, Nid (..), Packet,
                                            getClientType, regPacketRES)
import           System.Entropy            (getEntropy)
import           UnliftIO                  (MonadUnliftIO)

type ServerEnv serv =
  M.ServerEnv serv ClientConfig Nid Msgid (Packet Command)

startServer
  :: (Servable serv, Transport tp, Persist db, MonadUnliftIO m)
  => PersistConfig db
  -> (TransportConfig (STP serv) -> TransportConfig tp)
  -> S.ServerConfig serv
  -> m ()
startServer dbconfig mk config = do
  sEnv <- fmap mapEnv . initServerEnv config sessionGen mk $ \_ _ connEnv -> do
    (_ :: ClientType) <- getClientType <$> runConnT connEnv receive
    nid <- getEntropy 4
    runConnT connEnv $ send (regPacketRES $ Data nid)
    wFuncList <- IOMap.empty
    wJobQueue <- IOMap.empty
    return $ Just (Nid nid, ClientConfig {..})

  setDefaultSessionTimeout sEnv 100
  setKeepalive sEnv 500

  schedEnv <- initSchedEnv dbconfig $ runServerT sEnv stopServerT

  setOnNodeLeave sEnv $ \_ ClientConfig {..} ->
    runSchedT schedEnv $ do
      mapM_ failJob =<< IOMap.keys wJobQueue
      mapM_ removeFunc =<< IOMap.keys wFuncList

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
