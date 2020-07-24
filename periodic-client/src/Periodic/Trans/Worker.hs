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
import           Metro.Session                (getSessionId, receive, send)
import           Periodic.IOList              (IOList, newIOList)
import qualified Periodic.IOList              as IL (append, toListSTM)
import           Periodic.Node
import qualified Periodic.Trans.BaseClient    as BT (BaseClientEnv, checkHealth,
                                                     close, getClientEnv, ping)
import           Periodic.Trans.Job           (JobT, func_, name, workFail)
import           Periodic.Types               (ClientType (TypeWorker), Msgid,
                                               getClientType, getResult,
                                               packetREQ, regPacketREQ)
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand
import           Periodic.Types.WorkerCommand
import           System.Log.Logger            (errorM)
import           UnliftIO
import           UnliftIO.Concurrent          (threadDelay)

type TaskList tp m = IOHashMap FuncName (JobT tp m ())

type JobList = IOList (Msgid, Job)

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
  jobList <- newIOList
  taskSize <- newTVarIO 0

  jobEnv1 <- initEnv1 mapEnv connEnv Nothing (Nid nid) sessionGen

  let wEnv = WorkerEnv {..}

  runNodeT jobEnv1 $ do

    void $ async $ startNodeT $ assignJobHandler jobList
    runWorkerT wEnv $ do
      void . async $ forever $ do
        threadDelay $ 100 * 1000 * 1000
        checkHealth
      void . async . runJobT $ processJobQueue wEnv
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

getAssignJob :: ServerCommand -> Maybe Job
getAssignJob (JobAssign job) = Just job
getAssignJob _               = Nothing

work
  :: (MonadUnliftIO m, Transport tp)
  => Int -> WorkerT tp m ()
work size = do
  jl <- asks jobList
  tskSize <- asks taskSize
  runJobT $ do
    asyncs <- replicateM size $ async $ work_ jl tskSize size
    void $ waitAnyCancel asyncs

work_ :: (MonadUnliftIO m, Transport tp) => JobList -> TVar Int -> Int -> JobT tp m ()
work_ jl tskSize size = do
  sid <- nextSessionId
  jobEnv <- newSessionEnv (Just (-1)) sid
  runSessionT_ jobEnv $ do
    io0 <- async . forever $ do
      atomically $ do
        s <- readTVar tskSize
        when (s >= size) retrySTM

      send $ packetREQ GrabJob
      threadDelay 10000000 -- 10s

    io1 <- async . forever $ assignJobHandler jl

    void $ waitAnyCancel [io0, io1]

processJob :: (MonadUnliftIO m, Transport tp) => WorkerEnv tp m -> (Msgid, Job) -> JobT tp m ()
processJob WorkerEnv{..} (sid, job) = do
  atomically $ do
    s <- readTVar taskSize
    writeTVar taskSize (s + 1)
  withEnv (Just job) $ do
    f <- func_
    task <- HM.lookup taskList f
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

assignJobHandler :: (MonadIO m, Transport tp) => JobList -> SessionT u ServerCommand tp m ()
assignJobHandler jl = do
  pid <- getSessionId
  r <- getResult Nothing getAssignJob <$> receive
  case r of
    Nothing ->
      liftIO $ errorM "Periodic.Node" $ "Session [" ++ show pid ++ "] not found."
    Just job -> IL.append jl (pid, job)

processJobQueue :: (MonadUnliftIO m, Transport tp) => WorkerEnv tp m -> JobT tp m ()
processJobQueue wEnv@WorkerEnv {..} = forever $ do
  jobs <- atomically $ do
    v <- IL.toListSTM jobList
    if null v then retrySTM
              else return v

  mapM_ (async . processJob wEnv) jobs
