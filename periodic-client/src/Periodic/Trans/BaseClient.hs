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
  , checkHealth
  ) where

import           Control.Monad                (unless)
import           Data.ByteString              (ByteString)
import           Data.Int                     (Int64)
import           Data.Maybe                   (fromMaybe)
import           Metro.Class                  (Transport)
import           Metro.Node                   (getEnv1, request, stopNodeT)
import           Metro.Utils                  (getEpochTime)
import           Periodic.Node
import           Periodic.Types               (getResult, packetREQ)
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

submitJob_ :: (MonadUnliftIO m, Transport tp) => Job -> BaseClientT u tp m Bool
submitJob_ j = getResult False isSuccess <$> request Nothing (packetREQ (SubmitJob j))

submitJob
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobName -> Maybe Workload -> Maybe Int64 -> BaseClientT u tp m Bool
submitJob fn jn w later = do
  schedAt <- (+fromMaybe 0 later) <$> getEpochTime
  submitJob_ $ setSchedAt schedAt $ setWorkload (fromMaybe "" w) $ initJob fn jn

runJob_ :: (MonadUnliftIO m, Transport tp) => Job -> BaseClientT u tp m (Maybe ByteString)
runJob_ j =  getResult Nothing getData <$> request Nothing (packetREQ . RunJob $ setSchedAt 0 j)
  where getData :: ServerCommand -> Maybe ByteString
        getData (Data bs) = Just bs
        getData _         = Nothing

runJob
  :: (MonadUnliftIO m, Transport tp)
  => FuncName -> JobName -> Maybe Workload -> BaseClientT u tp m (Maybe ByteString)
runJob fn jn w = do
  schedAt <- getEpochTime
  runJob_ $ setSchedAt schedAt $ setWorkload (fromMaybe "" w) $ initJob fn jn

checkHealth :: (MonadUnliftIO m, Transport tp) => BaseClientT u tp m ()
checkHealth = do
  ret <- timeout 10000000 ping
  case ret of
    Nothing -> close
    Just r  -> unless r close

getClientEnv :: (Monad m, Transport tp) => BaseClientT u tp m (BaseClientEnv u tp)
getClientEnv = getEnv1
