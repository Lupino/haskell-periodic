{-# LANGUAGE OverloadedStrings #-}

module Periodic.Trans.BaseClient
  ( BaseClientT
  , BaseClientEnv
  , getClientEnv
  , close
  , runBaseClientT

  , ping
  , submitJob_
  , submitJob
  , runJob_
  , runJob
  , recvJobData_
  , recvJobData
  , checkHealth

  , successRequest
  ) where

import           Control.Monad                (unless)
import           Control.Monad.Trans.Class    (lift)
import           Data.Binary                  (Binary)
import           Data.ByteString              (ByteString)
import           Data.Int                     (Int64)
import           Metro.Class                  (Transport)
import           Metro.Node                   (getEnv1, request, stopNodeT,
                                               withSessionT)
import qualified Metro.Session                as S (receive, send)
import           Metro.Utils                  (foreverExit, getEpochTime)
import           Periodic.Node
import           Periodic.Types               (Packet, getResult, packetREQ)
import           Periodic.Types.ClientCommand
import           Periodic.Types.Job
import           Periodic.Types.ServerCommand
import           UnliftIO

type BaseClientEnv u = NodeEnv u ServerCommand
type BaseClientT u = NodeT u ServerCommand

runBaseClientT :: Monad m => BaseClientEnv u tp -> BaseClientT u tp m a -> m a
runBaseClientT = runNodeT

close :: (MonadUnliftIO m, Transport tp) => BaseClientT u tp m ()
close = stopNodeT

ping :: (MonadUnliftIO m, Transport tp) => BaseClientT u tp m Bool
ping = getResult False isPong <$> request Nothing (packetREQ Ping)

successRequest
  :: (MonadUnliftIO m, Transport tp, Binary a)
  => Packet a -> BaseClientT u tp m Bool
successRequest pkt = getResult False isSuccess <$> request Nothing pkt

submitJob_ :: (MonadUnliftIO m, Transport tp) => Job -> BaseClientT u tp m Bool
submitJob_ j = successRequest (packetREQ (SubmitJob j))

submitJob
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobName -> Workload -> Int64 -> Int -> BaseClientT u tp m Bool
submitJob fn jn w later tout = do
  schedAt <- (+later) <$> getEpochTime
  submitJob_
    $ setTimeout tout
    $ setSchedAt schedAt
    $ setWorkload w
    $ initJob fn jn

getServerData :: Maybe (Packet ServerCommand) -> Maybe ByteString
getServerData = getResult Nothing getData
  where getData :: ServerCommand -> Maybe ByteString
        getData (Data bs) = Just bs
        getData _         = Nothing

runJob_ :: (MonadUnliftIO m, Transport tp) => Job -> BaseClientT u tp m (Maybe ByteString)
runJob_ j =  getServerData <$> request Nothing (packetREQ . RunJob $ setSchedAt 0 j)

runJob
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobName -> Workload -> Int -> BaseClientT u tp m (Maybe ByteString)
runJob fn jn w tout = do
  runJob_ $ setTimeout tout $ setWorkload w $ initJob fn jn

recvJobData_
  :: (MonadUnliftIO m, Transport tp)
  => (ByteString -> m ()) ->  Job -> BaseClientT u tp m ()
recvJobData_ cb j = withSessionT Nothing $ do
  S.send (packetREQ (RecvData j))
  foreverExit $ \exit -> do
    ret <- lift S.receive
    case getServerData ret of
      Nothing    -> pure ()
      Just "EOF" -> exit ()
      Just dat   -> lift $ lift $ cb dat

recvJobData
  :: (MonadUnliftIO m, Transport tp)
  =>(ByteString -> m ()) ->  FuncName -> JobName -> BaseClientT u tp m ()
recvJobData cb fn jn = recvJobData_ cb $ initJob fn jn

checkHealth :: (MonadUnliftIO m, Transport tp) => BaseClientT u tp m ()
checkHealth = do
  ret <- timeout 10000000 ping
  case ret of
    Nothing -> close
    Just r  -> unless r close

getClientEnv :: (Monad m, Transport tp) => BaseClientT u tp m (BaseClientEnv u tp)
getClientEnv = getEnv1
