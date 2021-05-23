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
import           Data.IOMap                   (IOMap)
import qualified Data.IOMap                   as Map (delete, empty, insert,
                                                      lookup)
import qualified Data.IOMap.STM               as MapS (modifyIOMap, toList)
import qualified Data.Map.Strict              as RMap (empty)
import           Metro.Class                  (Transport, TransportConfig,
                                               getPacketId)
import           Metro.Conn                   (initConnEnv, runConnT)
import qualified Metro.Conn                   as Conn
import           Metro.Node                   (NodeMode (..), SessionMode (..),
                                               initEnv1, newSessionEnv,
                                               nextSessionId, runSessionT_,
                                               setDefaultSessionTimeout1,
                                               setNodeMode, setSessionMode,
                                               startNodeT_, withEnv,
                                               withSessionT)
import           Metro.Session                (send)
import           Periodic.Node
import qualified Periodic.Trans.BaseClient    as BT (BaseClientEnv, checkHealth,
                                                     close, getClientEnv, ping)
import           Periodic.Trans.Job           (JobT, func_, name, workFail)
import           Periodic.Types               (ClientType (TypeWorker), Msgid,
                                               Packet, getClientType, getResult,
                                               packetREQ, regPacketREQ)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand
import           System.Log.Logger            (errorM)
import           UnliftIO
import           UnliftIO.Concurrent          (threadDelay)

type TaskList tp m = IOMap FuncName (JobT tp m ())

type JobList = IOMap (Msgid, JobHandle) Job

data WorkerEnv tp m = WorkerEnv
    { taskList :: TaskList tp m
    , jobList  :: JobList
    , taskSize :: TVar Int
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
    Conn.receive

  let nid = case getClientType r of
              Data v -> v
              _      -> ""

  taskList <- Map.empty
  jobList <- Map.empty
  taskSize <- newTVarIO 0

  jobEnv1 <- initEnv1 mapEnv connEnv Nothing (Nid nid) sessionGen
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
  => FuncName -> JobT tp m () -> WorkerT tp m ()
addFunc f j = do
  runJobT $ withSessionT Nothing $ send (packetREQ $ CanDo f)
  ref <- asks taskList
  Map.insert f j ref

broadcast
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobT tp m () -> WorkerT tp m ()
broadcast f j = do
  runJobT $ withSessionT Nothing $ send (packetREQ $ Broadcast f)
  ref <- asks taskList
  Map.insert f j ref

removeFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> WorkerT tp m ()
removeFunc f = do
  tskList <- asks taskList
  runJobT $ removeFunc_ tskList f

removeFunc_
  :: (MonadUnliftIO m, Transport tp)
  => TaskList tp m -> FuncName -> JobT tp m ()
removeFunc_ ref f = do
  withSessionT Nothing $ send (packetREQ $ CantDo f)
  Map.delete f ref

getAssignJob :: ServerCommand -> Maybe Job
getAssignJob (JobAssign job) = Just job
getAssignJob _               = Nothing

work
  :: (MonadUnliftIO m, Transport tp)
  => Int -> WorkerT tp m ()
work size = do
  tskSize <- asks taskSize
  runJobT $ do
    envs <- mapM (newSessionEnv (Just (-1))) =<< replicateM size nextSessionId
    forever $ do
      atomically $ do
        s <- readTVar tskSize
        when (s >= size) retrySTM

      mapM_ (`runSessionT_` (send $ packetREQ GrabJob)) envs
      threadDelay 10000000 -- 10s


processJob :: (MonadUnliftIO m, Transport tp) => WorkerEnv tp m -> ((Msgid, JobHandle), Job) -> JobT tp m ()
processJob WorkerEnv{..} ((sid, _), job) = do
  atomically $ do
    s <- readTVar taskSize
    writeTVar taskSize (s + 1)
  withEnv (Just job) $ do
    f <- func_
    task <- Map.lookup f taskList
    case task of
      Nothing -> do
        removeFunc_ taskList f
        workFail
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
          workFail

  atomically $ do
    s <- readTVar taskSize
    writeTVar taskSize (s - 1)

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
