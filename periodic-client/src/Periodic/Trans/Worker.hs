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
import           Data.Maybe                   (isJust)
import           Metro.Class                  (Transport, TransportConfig)
import           Metro.Conn                   (initConnEnv, runConnT)
import qualified Metro.Conn                   as Conn
import           Metro.IOHashMap              (IOHashMap, newIOHashMap)
import qualified Metro.IOHashMap              as HM (delete, insert, lookup)
import           Metro.Node                   (NodeMode (..), SessionMode (..),
                                               initEnv1, newSessionEnv,
                                               nextSessionId, runSessionT_,
                                               setDefaultSessionTimeout,
                                               setNodeMode, setSessionMode,
                                               startNodeT, withEnv,
                                               withSessionT)
import           Metro.Session                (readerSize, receive, send)
import           Periodic.IOList              (IOList, newIOList)
import           Periodic.Node
import qualified Periodic.Trans.BaseClient    as BT (BaseClientEnv, checkHealth,
                                                     close, getClientEnv, ping)
import           Periodic.Trans.Job           (JobEnv, JobT, func_, name,
                                               workFail)
import           Periodic.Types               (ClientType (TypeWorker),
                                               getClientType, getResult,
                                               packetREQ, regPacketREQ)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand
import           System.Log.Logger            (errorM)
import           UnliftIO
import           UnliftIO.Concurrent          (threadDelay)

type TaskList tp m = IOHashMap FuncName (JobT tp m ())

data WorkerEnv tp m = WorkerEnv
    { taskList :: TaskList tp m
    , jobQueue :: IOList Job
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
  askUnliftIO = WorkerT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runWorkerT r))
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

  taskList <- newIOHashMap
  jobQueue <- newIOList

  jobEnv1 <- initEnv1 mapEnv connEnv Nothing (Nid nid) sessionGen

  runNodeT jobEnv1 $ do

    void $ async $ startNodeT defaultSessionHandler
    runWorkerT WorkerEnv {..} $ do
      void . async $ forever $ do
        threadDelay $ 100 * 1000 * 1000
        checkHealth
      m

  where mapEnv =
          setNodeMode Multi
          . setSessionMode SingleAction
          . setDefaultSessionTimeout 100


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
  HM.insert ref f j

broadcast
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobT tp m () -> WorkerT tp m ()
broadcast f j = do
  runJobT $ withSessionT Nothing $ send (packetREQ $ Broadcast f)
  ref <- asks taskList
  HM.insert ref f j

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
  HM.delete ref f

grabJob
  :: (MonadUnliftIO m, Transport tp)
  => JobEnv -> JobT tp m (Maybe Job)
grabJob jobEnv = do
  pl <- runSessionT_ jobEnv $ do
    size <- readerSize
    when (size == 0) $ send $ packetREQ GrabJob
    timeout 10000000 receive

  case pl of
    Nothing   -> pure Nothing
    Just rpkt -> pure $ getResult Nothing getAssignJob rpkt

getAssignJob :: ServerCommand -> Maybe Job
getAssignJob (JobAssign job) = Just job
getAssignJob _               = Nothing

work
  :: (MonadUnliftIO m, Transport tp)
  => Int -> WorkerT tp m ()
work size = do
  tskList <- asks taskList
  runJobT $ do
    asyncs <- replicateM size $ async $ work_ tskList
    void $ waitAnyCancel asyncs

work_ :: (MonadUnliftIO m, Transport tp) => TaskList tp m -> JobT tp m ()
work_ tskList = do
  sid <- nextSessionId
  jobEnv <- newSessionEnv (Just (-1)) sid
  forever $ do
    j <- grabJob jobEnv
    processJob tskList j

processJob :: (MonadUnliftIO m, Transport tp) => TaskList tp m -> Maybe Job -> JobT tp m ()
processJob tskList mjob =
  when (isJust mjob) $
    withEnv mjob $ do
      f <- func_
      task <- HM.lookup tskList f
      case task of
        Nothing -> do
          removeFunc_ tskList f
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

checkHealth
  :: (MonadUnliftIO m, Transport tp)
  => WorkerT tp m ()
checkHealth = runJobT BT.checkHealth
