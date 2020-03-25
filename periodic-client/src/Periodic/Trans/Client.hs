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

import           Control.Monad                (forever, void)
import           Data.Binary                  (decode)
import           Data.ByteString              (ByteString)
import           Data.ByteString.Lazy         (fromStrict)
import           Metro.Class                  (Transport, TransportConfig)
import           Metro.Conn                   (initConnEnv, runConnT)
import qualified Metro.Conn                   as Conn
import           Metro.Node                   (NodeMode (..), SessionMode (..),
                                               initEnv1, request,
                                               setDefaultSessionTimeout,
                                               setNodeMode, setSessionMode,
                                               startNodeT, withSessionT)
import           Metro.Session                (send)
import           Periodic.Node
import           Periodic.Trans.BaseClient
import           Periodic.Types               (ClientType (TypeClient),
                                               getClientType, getResult,
                                               packetREQ, regPacketREQ)
import           Periodic.Types.ClientCommand
import           Periodic.Types.Internal      (ConfigKey (..))
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand
import           UnliftIO
import           UnliftIO.Concurrent          (threadDelay)

type ClientEnv = BaseClientEnv ()
type ClientT = BaseClientT ()

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

dropFunc
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> BaseClientT u tp m Bool
dropFunc func = getResult False isSuccess <$> request Nothing (packetREQ (DropFunc func))

removeJob
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobName -> BaseClientT u tp m Bool
removeJob f n = getResult False isSuccess <$> request Nothing (packetREQ (RemoveJob f n))

status
  :: (MonadUnliftIO m, Transport tp)
  => BaseClientT u tp m ByteString
status = getResult "" getRaw <$> request Nothing (packetREQ Status)
  where getRaw :: ServerCommand -> ByteString
        getRaw (Data bs) = bs
        getRaw _         = ""

configGet
  :: (MonadUnliftIO m, Transport tp)
  => String -> BaseClientT u tp m Int
configGet k = getResult 0 getV <$> request Nothing (packetREQ (ConfigGet (ConfigKey k)))
  where getV :: ServerCommand -> Int
        getV (Config v) = v
        getV _          = 0

configSet
  :: (MonadUnliftIO m, Transport tp)
  => String -> Int -> BaseClientT u tp m Bool
configSet k v = getResult False isSuccess <$> request Nothing (packetREQ (ConfigSet (ConfigKey k) v))

load :: (MonadUnliftIO m, Transport tp) => [Job] -> BaseClientT u tp m Bool
load jobs = getResult False isSuccess <$> request Nothing (packetREQ (Load jobs))

dump :: (MonadUnliftIO m, Transport tp) => BaseClientT u tp m [Job]
dump = getResult [] getV <$> request Nothing (packetREQ Dump)
  where getV :: ServerCommand -> [Job]
        getV (Data bs) = decode $ fromStrict bs
        getV _         = []

shutdown :: (MonadUnliftIO m, Transport tp) => BaseClientT u tp m ()
shutdown = withSessionT Nothing $ send $ packetREQ Shutdown
