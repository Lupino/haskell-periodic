{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server
  ( startServer
  ) where


import           Control.Monad                (when)
import           Control.Monad.Trans.Class    (lift)
import           Data.Binary.Get              (getWord32be, runGet)
import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (fromStrict)
import qualified Data.IOMap                   as IOMap
import           Data.Map.Strict              (filterWithKey)
import qualified Data.Set                     as Set
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
import           Periodic.Server.Auth         (FuncAuth, startFuncAuthReloader)
import           Periodic.Types               (ClientType (..), Job,
                                               Msgid (..), Nid (..), Packet,
                                               getClientType, getHandle,
                                               getResult, getTimeout,
                                               packetRES, regPacketRES)
import           Periodic.Types.ServerCommand (ServerCommand (JobAssign))
import           Periodic.Types.ClientType    (ClientIdentity)
import           Periodic.Types.WorkerCommand (getJobAssignResult)
import           System.Entropy               (getEntropy)
import           System.Log.Logger            (errorM)
import           System.Timeout               (timeout)
import           UnliftIO                     (MonadUnliftIO, atomically,
                                               liftIO, newTVarIO, readTVarIO,
                                               tryAny, writeTVar)

type ServerEnv serv =
  M.ServerEnv serv ClientConfig Nid Msgid (Packet Command)

getJobAssignResultCmd :: Command -> Maybe Bool
getJobAssignResultCmd (WC cmd) = getJobAssignResult cmd
getJobAssignResultCmd _        = Nothing

doAssignJob :: Transport tp => ServerEnv serv tp -> Int -> Nid -> Msgid -> Job -> IO Bool
doAssignJob sEnv wait nid msgid job = do
  menv0 <- IOMap.lookup nid $ getNodeEnvList sEnv
  case menv0 of
    Nothing   -> return False
    Just env0 -> do
      assigned <- newTVarIO Nothing
      mr <- timeout waitS
         $ tryAny
         $ runNodeT1 env0
         $ withSessionT_ (pure msgid) Nothing
         $ do

        S.send (packetRES (JobAssign job))
        foreverExit $ \exit -> do
          ret <- lift $ getResult Nothing getJobAssignResultCmd <$> S.receive
          case ret of
            Nothing -> pure ()
            Just r -> do
              atomically $ writeTVar assigned (Just r)
              exit ()

      isTransportError <- case mr of
        Nothing -> do
          errorM "Periodic.Server" "Job assignment timed out. Consider increasing the 'assign-wait' configuration value to allow more time."
          pure True
        Just (Left err) -> do
          errorM "Periodic.Server" $ "Job assignment error: " ++ show err
          pure True
        _ -> pure False

      mrAssigned <- readTVarIO assigned
      let r = mrAssigned == Just True
      when r $ do
        env1 <- runNodeT1 env0 env
        expiredAt <- (+tout) <$> getEpochTime
        IOMap.insert jh expiredAt (wJobQueue env1)

      when (isTransportError && not r) $ runNodeT1 env0 stopNodeT

      return r

  where jh = getHandle job
        tout = fromIntegral $ getTimeout job
        waitS = wait * 1000000

doPushData :: Transport tp => ServerEnv serv tp -> Nid -> Msgid -> ByteString -> IO ()
doPushData sEnv nid msgid w = do
  menv0 <- IOMap.lookup nid $ getNodeEnvList sEnv
  case menv0 of
    Nothing   -> return ()
    Just env0 -> do
      r <- tryAny
        $ runConnT (connEnv env0)
        $ send
        $ setPacketId msgid
        $ packetRES (Data w)
      case r of
        Left err -> errorM "Periodic.Server" $ "Push data failed: " ++ show err
        Right _  -> pure ()

startServer
  :: (Servable serv, Transport tp, Persist db, MonadUnliftIO m)
  => PersistConfig db
  -> (TransportConfig (STP serv) -> TransportConfig tp)
  -> S.ServerConfig serv
  -> Hook db
  -> Maybe FuncAuth
  -> Int
  -> Int
  -> m ()
startServer dbconfig mk config hook mAuth pushTaskSize schedTaskSize = do
  mapM_ (liftIO . startFuncAuthReloader) mAuth
  grabQueue <- newGrabQueue
  sEnv <- fmap mapEnv . initServerEnv config sessionGen mk $ \_ _ connEnv0 -> do
    mClientType <- tryAny $ getClientType <$> runConnT connEnv0 receive_
    case mClientType of
      Left err -> do
        errorM "Periodic.Server" $ "Client registration decode error: " ++ show err
        pure Nothing
      Right clientType -> do
        nid <- getEntropy 4
        runConnT connEnv0 $ send (regPacketRES $ Data nid)
        let nidV = Nid $! runGet getWord32be $ fromStrict nid
            wAuth = mAuth
            wIdentity = clientIdentity clientType
        wFuncList  <- newTVarIO Set.empty
        wJobQueue  <- IOMap.empty
        wMsgidList <- newTVarIO []
        pushAgent grabQueue wFuncList nidV wMsgidList
        pure $ Just (nidV, ClientConfig {..})

  setDefaultSessionTimeout sEnv 60
  setKeepalive sEnv 120

  schedEnv <- initSchedEnv dbconfig grabQueue
              (runServerT sEnv stopServerT)
              (doAssignJob sEnv) (doPushData sEnv) hook (getKeepalive sEnv) (getMaxPoolSize sEnv)

  setOnNodeLeave sEnv $ \nid ClientConfig {..} ->
    runSchedT schedEnv $ do
      mapM_ failJob =<< IOMap.keys wJobQueue
      mapM_ removeFunc . Set.toList =<< readTVarIO wFuncList
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

        clientIdentity :: ClientType -> Maybe ClientIdentity
        clientIdentity (TypeAuthClient ident) = Just ident
        clientIdentity (TypeAuthWorker ident) = Just ident
        clientIdentity _                      = Nothing
