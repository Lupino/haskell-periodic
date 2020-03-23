{-# LANGUAGE OverloadedStrings #-}

module Periodic.Trans.Client
  ( ClientT
  , ClientEnv
  , open
  , close
  , runClientT

  , ping
  , submitJob_
  , submitJob
  , runJob_
  , runJob
  , removeJob
  , dropFunc
  , status
  , configGet
  , configSet
  , load
  , dump
  , shutdown
  ) where

import           Control.Monad                (forever, unless, void)
import           Data.Binary                  (decode)
import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (fromStrict)
import           Data.Int                     (Int64)
import           Data.Maybe                   (fromMaybe)
import           Metro.Class                  (Transport, TransportConfig)
import           Metro.Conn                   (initConnEnv, runConnT)
import qualified Metro.Conn                   as Conn
import           Metro.Node                   (NodeMode (..), SessionMode (..),
                                               initEnv1, request,
                                               setDefaultSessionTimeout,
                                               setNodeMode, setSessionMode,
                                               startNodeT, stopNodeT,
                                               withSessionT)
import           Metro.Session                (send)
import           Metro.Utils                  (getEpochTime)
import           Periodic.Node
import           Periodic.Types               (ClientType (TypeClient),
                                               getClientType, getResult,
                                               packetREQ, regPacketREQ)
import           Periodic.Types.ClientCommand
import           Periodic.Types.Internal      (ConfigKey (..))
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand
import           UnliftIO
import           UnliftIO.Concurrent          (threadDelay)

type ClientEnv = NodeEnv () ServerCommand
type ClientT = NodeT () ServerCommand
type ClientSessionT = SessionT () ServerCommand

runClientT :: Monad m => ClientEnv tp -> ClientT tp m a -> m a
runClientT = runNodeT

open
  :: (MonadUnliftIO m, Transport tp)
  => TransportConfig tp -> m (ClientEnv tp)
open config = do
  connEnv <- initConnEnv config
  r <- runConnT connEnv $ do
    Conn.send $ regPacketREQ TypeClient
    Conn.receive

  let nid = case getClientType r of
              Data v -> v
              _      -> ""

  clientEnv <- initEnv1 mapEnv connEnv () (Nid nid) sessionGen

  runClientT clientEnv $ do
    void . async $ forever $ do
      threadDelay $ 100 * 1000 * 1000
      checkHealth

    void $ async $ startNodeT defaultSessionHandler
  return clientEnv

  where mapEnv =
          setNodeMode Multi
          . setSessionMode SingleAction
          . setDefaultSessionTimeout 100

close :: (MonadUnliftIO m, Transport tp) => ClientT tp m ()
close = stopNodeT

ping :: (MonadUnliftIO m, Transport tp) => ClientT tp m Bool
ping = getResult False isPong <$> request Nothing (packetREQ Ping)

submitJob_ :: (MonadUnliftIO m, Transport tp) => Job -> ClientT tp m Bool
submitJob_ j = getResult False isSuccess <$> request Nothing (packetREQ (SubmitJob j))

submitJob
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobName -> Maybe Workload -> Maybe Int64 -> ClientT tp m Bool
submitJob fn jn w later = do
  schedAt <- (+fromMaybe 0 later) <$> getEpochTime
  submitJob_ $ setSchedAt schedAt $ setWorkload (fromMaybe "" w) $ initJob fn jn

runJob_ :: (MonadUnliftIO m, Transport tp) => Job -> ClientT tp m (Maybe ByteString)
runJob_ j =  getResult Nothing getData <$> request Nothing (packetREQ . RunJob $ setSchedAt 0 j)
  where getData :: ServerCommand -> Maybe ByteString
        getData (Data bs) = Just bs
        getData _         = Nothing

runJob
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobName -> Maybe Workload -> ClientT tp m (Maybe ByteString)
runJob fn jn w = do
  schedAt <- getEpochTime
  runJob_ $ setSchedAt schedAt $ setWorkload (fromMaybe "" w) $ initJob fn jn

dropFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> ClientT tp m Bool
dropFunc func = getResult False isSuccess <$> request Nothing (packetREQ (DropFunc func))

removeJob
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobName -> ClientT tp m Bool
removeJob f n = getResult False isSuccess <$> request Nothing (packetREQ (RemoveJob f n))

status
  :: (MonadUnliftIO m, Transport tp)
  => ClientT tp m ByteString
status = getResult "" getRaw <$> request Nothing (packetREQ Status)
  where getRaw :: ServerCommand -> ByteString
        getRaw (Data bs) = bs
        getRaw _         = ""

configGet
  :: (MonadUnliftIO m, Transport tp)
  => String -> ClientT tp m Int
configGet k = getResult 0 getV <$> request Nothing (packetREQ (ConfigGet (ConfigKey k)))
  where getV :: ServerCommand -> Int
        getV (Config v) = v
        getV _          = 0

configSet
  :: (MonadUnliftIO m, Transport tp)
  => String -> Int -> ClientT tp m Bool
configSet k v = getResult False isSuccess <$> request Nothing (packetREQ (ConfigSet (ConfigKey k) v))

load :: (MonadUnliftIO m, Transport tp) => [Job] -> ClientT tp m Bool
load jobs = getResult False isSuccess <$> request Nothing (packetREQ (Load jobs))

dump :: (MonadUnliftIO m, Transport tp) => ClientT tp m [Job]
dump = getResult [] getV <$> request Nothing (packetREQ Dump)
  where getV :: ServerCommand -> [Job]
        getV (Data bs) = decode $ fromStrict bs
        getV _         = []

shutdown :: (MonadUnliftIO m, Transport tp) => ClientT tp m ()
shutdown = withSessionT Nothing $ send $ packetREQ Shutdown

checkHealth :: (MonadUnliftIO m, Transport tp) => ClientT tp m ()
checkHealth = do
  ret <- timeout 10000000 ping
  case ret of
    Nothing -> close
    Just r  -> unless r close
