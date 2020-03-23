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
  , runWorkerT
  , ping
  , addFunc
  , broadcast
  , removeFunc
  , work
  , close
  ) where

import           Control.Monad                (forever, replicateM, unless,
                                               void, when)
import           Control.Monad.Reader.Class   (MonadReader (ask))
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
                                               nextSessionId, request,
                                               runSessionT_,
                                               setDefaultSessionTimeout,
                                               setNodeMode, setSessionMode,
                                               startNodeT, stopNodeT, withEnv,
                                               withSessionT)
import           Metro.Session                (readerSize, receive, send)
import           Periodic.Node
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

data WorkerEnv tp m = WorkerEnv (IOHashMap FuncName (JobT tp m ()))

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
        return (UnliftIO (unliftIO u . runWorkerT_ r))
  withRunInIO inner = WorkerT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runWorkerT_ r)

instance MonadTrans (WorkerT tp) where
  lift = WorkerT . lift . lift

runWorkerT_ :: WorkerEnv tp m -> WorkerT tp m a -> JobT tp m a
runWorkerT_ workerEnv = flip runReaderT workerEnv . unWorkerT

-- type WorkerT tp m = NodeT (TaskList tp m) ServerCommand tp m
-- type WSessionEnv = SessionEnv ServerCommand

runWorkerT
  :: (MonadUnliftIO m, Transport tp)
  => TransportConfig tp -> WorkerT tp m () -> m ()
runWorkerT config m = do
  connEnv <- initConnEnv config
  r <- runConnT connEnv $ do
    Conn.send $ regPacketREQ TypeWorker
    Conn.receive

  let nid = case getClientType r of
              Data v -> v
              _      -> ""

  workerEnv <- WorkerEnv <$> newIOHashMap
  jobEnv1 <- initEnv1 mapEnv connEnv Nothing (Nid nid) sessionGen

  runNodeT jobEnv1 $ do

    void $ async $ startNodeT defaultSessionHandler
    runWorkerT_ workerEnv $ do
      void . async $ forever $ do
        threadDelay $ 100 * 1000 * 1000
        checkHealth
      m

  where mapEnv =
          setNodeMode Multi
          . setSessionMode SingleAction
          . setDefaultSessionTimeout 100


liftJob :: Monad m => JobT tp m a -> WorkerT tp m a
liftJob = WorkerT . lift

close :: (MonadUnliftIO m, Transport tp) => WorkerT tp m ()
close = liftJob stopNodeT

ping :: (MonadUnliftIO m, Transport tp) => WorkerT tp m Bool
ping = liftJob $ getResult False isPong <$> request Nothing (packetREQ Ping)

addFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobT tp m () -> WorkerT tp m ()
addFunc f j = do
  liftJob $ withSessionT Nothing $ send (packetREQ $ CanDo f)
  WorkerEnv ref <- ask
  HM.insert ref f j

broadcast
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobT tp m () -> WorkerT tp m ()
broadcast f j = do
  liftJob $ withSessionT Nothing $ send (packetREQ $ Broadcast f)
  WorkerEnv ref <- ask
  HM.insert ref f j

removeFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> WorkerT tp m ()
removeFunc f = do
  wEnv <- ask
  liftJob $ removeFunc_ wEnv f

removeFunc_
  :: (MonadUnliftIO m, Transport tp)
  => WorkerEnv tp m -> FuncName -> JobT tp m ()
removeFunc_ (WorkerEnv ref) f = do
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

  where getAssignJob :: ServerCommand -> Maybe Job
        getAssignJob (JobAssign job) = Just job
        getAssignJob _               = Nothing


work
  :: (MonadUnliftIO m, Transport tp)
  => Int -> WorkerT tp m ()
work size = do
  wEnv <- ask
  liftJob $ do
    asyncs <- replicateM size $ async $ work_ wEnv
    void $ waitAnyCancel asyncs

work_ :: (MonadUnliftIO m, Transport tp) => WorkerEnv tp m -> JobT tp m ()
work_ (WorkerEnv taskList) = do
  sid <- nextSessionId
  jobEnv <- newSessionEnv Nothing sid
  forever $ do
    j <- grabJob jobEnv
    when (isJust j) $
      withEnv j $ do
        f <- func_
        task <- HM.lookup taskList f
        case task of
          Nothing -> do
            removeFunc_ (WorkerEnv taskList) f
            workFail
          Just task' ->
            catchAny task' $ \e -> do
              n <- name
              liftIO $ errorM "Periodic.Worker"
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
checkHealth = do
  ret <- timeout 10000000 ping
  case ret of
    Nothing -> close
    Just r  -> unless r close
