{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Periodic.Trans.Worker
  ( WorkerT
  , startWorkerT
  , ping
  , addFunc
  , broadcast
  , removeFunc
  , work
  , close
  , runJobT
  , getClientEnv
  ) where


import           Control.Monad                (forever, replicateM, void, when)
import           Control.Monad.Reader.Class   (MonadReader, asks)
import           Control.Monad.Trans.Class    (MonadTrans, lift)
import           Control.Monad.Trans.Reader   (ReaderT (..), runReaderT)
import           Data.Binary                  (Binary)
import           Data.Binary.Get              (getWord32be, runGet)
import           Data.ByteString.Lazy         (fromStrict)
import           Data.Int                     (Int64)
import           Data.IOMap                   (IOMap)
import qualified Data.IOMap                   as Map (delete, empty,
                                                      foldrWithKey', insert,
                                                      lookup)
import qualified Data.IOMap.STM               as MapS (modifyIOMap, toList)
import qualified Data.Map.Strict              as RMap (empty)
import           Metro.Class                  (RecvPacket (..), Transport,
                                               TransportConfig, getPacketId)
import           Metro.Conn                   (initConnEnv, runConnT)
import qualified Metro.Conn                   as Conn
import           Metro.Node                   (NodeMode (..), SessionMode (..),
                                               initEnv1, newSessionEnv,
                                               nextSessionId, runSessionT_,
                                               setDefaultSessionTimeout1,
                                               setNodeMode, setSessionMode,
                                               startNodeT_, withEnv)
import           Metro.Session                (send)
import           Metro.Utils                  (getEpochTime)
import           Periodic.Node
import qualified Periodic.Trans.BaseClient    as BT (BaseClientEnv, checkHealth,
                                                     close, getClientEnv, ping,
                                                     successRequest)
import           Periodic.Trans.Job           (JobT, func_, name, workFail)
import           Periodic.Types               (ClientType (TypeWorker), Msgid,
                                               Packet, getClientType, getResult,
                                               packetREQ, recvRawPacket,
                                               regPacketREQ)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand
import           System.Log.Logger            (errorM)
import           UnliftIO
import           UnliftIO.Concurrent          (threadDelay)

type TaskList tp m = IOMap FuncName (JobT tp m ())

type JobList = IOMap (Msgid, JobHandle) Job

type GrabList = IOMap Msgid Int64

instance (Binary a) => RecvPacket (Maybe Job) (Packet a) where
  recvPacket _ = recvRawPacket

data WorkerEnv tp m = WorkerEnv
    { taskList :: TaskList tp m
    , jobList  :: JobList
    , tskSizeH :: TVar Int
    , maxSizeH :: TVar Int
    , grabList :: GrabList
    }

newtype WorkerT tp m a = WorkerT {unWorkerT :: ReaderT (WorkerEnv tp m) (JobT tp m) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (WorkerEnv tp m)
    )

instance MonadUnliftIO m => MonadUnliftIO (WorkerT tp m) where
  withRunInIO inner = WorkerT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runWorkerT r)

instance MonadTrans (WorkerT tp) where
  lift = WorkerT . lift . lift

runWorkerT :: WorkerEnv tp m -> WorkerT tp m a -> JobT tp m a
runWorkerT workerEnv = flip runReaderT workerEnv . unWorkerT

startWorkerT
  :: (MonadUnliftIO m, Transport tp)
  => TransportConfig tp -> WorkerT tp m () -> m ()
startWorkerT config m = do
  connEnv <- initConnEnv config
  r <- runConnT connEnv $ do
    Conn.send $ regPacketREQ TypeWorker
    Conn.receive_

  let nid = case getClientType r of
              Data v -> runGet getWord32be $ fromStrict v
              _      -> 0

  taskList <- Map.empty
  jobList  <- Map.empty
  tskSizeH <- newTVarIO 0
  maxSizeH <- newTVarIO 0
  grabList <- Map.empty

  jobEnv1 <- initEnv1 mapEnv connEnv Nothing (Nid nid) True sessionGen
  setDefaultSessionTimeout1 jobEnv1 100

  let wEnv = WorkerEnv {..}

  runNodeT jobEnv1 $ do

    void $ async $ startNodeT_ (filterPacketM jobList) defaultSessionHandler
    runWorkerT wEnv $ do
      void . async $ forever $ do
        threadDelay $ 100 * 1000 * 1000
        checkHealth
      void . async . runJobT $ processJobQueue wEnv
      m

  where mapEnv =
          setNodeMode Multi
          . setSessionMode SingleAction

filterPacketM :: MonadIO m => JobList -> Packet ServerCommand -> m Bool
filterPacketM jl rpkt = do
  case getResult Nothing getAssignJob (Just rpkt) of
    Nothing  -> return True
    Just job -> do
      Map.insert (getPacketId rpkt, getHandle job) job jl
      return False


runJobT :: Monad m => JobT tp m a -> WorkerT tp m a
runJobT = WorkerT . lift

getClientEnv
  :: (Monad m, Transport tp)
  => WorkerT tp m (BT.BaseClientEnv (Maybe Job) tp)
getClientEnv = runJobT BT.getClientEnv

close :: (MonadUnliftIO m, Transport tp) => WorkerT tp m ()
close = runJobT BT.close

ping :: (MonadUnliftIO m, Transport tp) => WorkerT tp m Bool
ping = runJobT BT.ping

addFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobT tp m () -> WorkerT tp m Bool
addFunc f j = do
  r <- runJobT $ BT.successRequest (packetREQ $ CanDo f)
  ref <- asks taskList
  Map.insert f j ref
  pure r

broadcast
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobT tp m () -> WorkerT tp m Bool
broadcast f j = do
  r <- runJobT $ BT.successRequest (packetREQ $ Broadcast f)
  ref <- asks taskList
  Map.insert f j ref
  pure r

removeFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> WorkerT tp m Bool
removeFunc f = do
  tskList <- asks taskList
  runJobT $ removeFunc_ tskList f

removeFunc_
  :: (MonadUnliftIO m, Transport tp)
  => TaskList tp m -> FuncName -> JobT tp m Bool
removeFunc_ ref f = do
  r <- BT.successRequest (packetREQ $ CantDo f)
  Map.delete f ref
  return r

getAssignJob :: ServerCommand -> Maybe Job
getAssignJob (JobAssign job) = Just job
getAssignJob _               = Nothing

nextGrab :: (Transport tp, MonadUnliftIO m) => GrabList -> JobT tp m ()
nextGrab gl = do
  v <- Map.foldrWithKey' foldFunc Nothing gl
  case v of
    Nothing -> pure ()
    Just (msgid, ts) -> do
      now <- getEpochTime
      when (ts + 300 < now) $ do
        jobEnv <- newSessionEnv (Just (-1)) msgid
        runSessionT_ jobEnv $ send (packetREQ GrabJob)
        Map.insert msgid now gl

  where foldFunc :: Msgid -> Int64 -> Maybe (Msgid, Int64) -> Maybe (Msgid, Int64)
        foldFunc msgid ts Nothing = Just (msgid, ts)
        foldFunc msgid ts (Just (msgid0, ts0))
          | ts0 > ts  = Just (msgid, ts)
          | otherwise = Just (msgid0, ts0)

work
  :: (MonadUnliftIO m, Transport tp)
  => Int -> WorkerT tp m ()
work size = do
  maxSize <- asks maxSizeH
  gl      <- asks grabList

  atomically $ writeTVar maxSize size

  runJobT $ do
    mapM_ (\msgid -> Map.insert msgid 0 gl) =<< replicateM size nextSessionId
    forever $ do
      nextGrab gl
      threadDelay 60000000 -- 60s


processJob :: (MonadUnliftIO m, Transport tp) => WorkerEnv tp m -> ((Msgid, JobHandle), Job) -> JobT tp m ()
processJob WorkerEnv{..} ((sid, _), job) = do
  atomically $ do
    s <- readTVar tskSizeH
    maxSize <- readTVar maxSizeH
    when (maxSize <= s) retrySTM
    writeTVar tskSizeH (s + 1)

  nextGrab grabList

  withEnv (Just job) $ do
    f <- func_
    task <- Map.lookup f taskList
    case task of
      Nothing -> do
        void $ removeFunc_ taskList f
        void workFail
      Just task' ->
        catchAny task' $ \e -> do
          n <- name
          liftIO $ errorM "Periodic.Trans.Worker"
                 $ concat [ "Failing on running job { name = "
                          , n
                          , ", "
                          , show f
                          , " }"
                          , "\nError: "
                          , show e
                          ]
          void workFail

  atomically $ do
    s <- readTVar tskSizeH
    writeTVar tskSizeH (s - 1)

  jobEnv <- newSessionEnv (Just (-1)) sid
  runSessionT_ jobEnv $ send (packetREQ GrabJob)

checkHealth
  :: (MonadUnliftIO m, Transport tp)
  => WorkerT tp m ()
checkHealth = runJobT BT.checkHealth

processJobQueue :: (MonadUnliftIO m, Transport tp) => WorkerEnv tp m -> JobT tp m ()
processJobQueue wEnv@WorkerEnv {..} = forever $ do
  jobs <- atomically $ do
    v <- MapS.toList jobList
    if null v then retrySTM
              else do
                MapS.modifyIOMap (const RMap.empty) jobList
                return v

  mapM_ (async . processJob wEnv) jobs
