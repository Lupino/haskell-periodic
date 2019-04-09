{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Periodic.Agent
  ( AgentReader
  , Msgid
  , AgentEnv (..)
  , AgentT
  , runAgentT
  , mkAgentReader
  , send
  , send_
  , msgid
  , msgidLength
  , aAlive
  , feed
  , receive
  , receive_
  , readerSize

  , AgentEnv'
  , agentEnv'
  , agentid
  , runAgentT'
  ) where

import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.Byteable              (Byteable (..))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B (concat, empty)
import           Periodic.Connection        (ConnEnv, ConnectionT,
                                             FromConn (..), connid',
                                             runConnectionT, statusTVar)
import qualified Periodic.Connection        as Conn (send)
import           Periodic.Transport         (Transport)
import           Periodic.Types             (Error (..))
import           Periodic.Types.Internal
import           System.Log.Logger          (errorM)
import           UnliftIO

type AgentReader = TVar [ByteString]
type Msgid = ByteString

data AgentEnv = AgentEnv
  { agentReader :: AgentReader
  , agentMsgid  :: Msgid
  }

newtype AgentT tp m a = AgentT { unAgentT :: ReaderT AgentEnv (ConnectionT tp m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AgentEnv)

instance MonadTrans (AgentT tp) where
  lift = AgentT . lift . lift

instance MonadUnliftIO m => MonadUnliftIO (AgentT tp m) where
  askUnliftIO = AgentT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runAgentT r))
  withRunInIO inner = AgentT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runAgentT r)

instance FromConn AgentT where
  fromConn = AgentT . lift

runAgentT :: AgentEnv -> AgentT tp m a -> ConnectionT tp m a
runAgentT aEnv = flip runReaderT aEnv . unAgentT

mkAgentReader :: MonadIO m => [ByteString] -> m AgentReader
mkAgentReader = newTVarIO

msgid :: Monad m => AgentT tp m Msgid
msgid = asks agentMsgid

msgidLength :: Int
msgidLength = 4

aAlive :: MonadIO m => AgentT tp m Bool
aAlive = readTVarIO =<< fromConn statusTVar

send_ :: (MonadUnliftIO m, Transport tp) => ByteString -> AgentT tp m ()
send_ pl = do
  mid <- msgid
  fromConn $ Conn.send $ B.concat [mid, pl]

send :: (Byteable cmd, Validatable cmd, MonadUnliftIO m, Transport tp) => cmd -> AgentT tp m ()
send cmd =
  case validate cmd of
    Right _ -> send_ $ toBytes cmd
    Left e  -> do
      liftIO $ errorM "Periodic.Agent" $ "InValidError " ++ e
      throwIO $ InValidError e

feed :: (MonadIO m) => ByteString -> AgentT tp m ()
feed dat = do
  reader <- asks agentReader
  atomically . modifyTVar' reader $ \v -> v ++ [dat]

receive_ :: (MonadIO m, Transport tp) => AgentT tp m ByteString
receive_ = do
  reader <- asks agentReader
  st <- fromConn statusTVar
  atomically $ do
    v <- readTVar reader
    if null v then do
      s <- readTVar st
      if s then retrySTM
           else pure B.empty
    else do
      writeTVar reader $! tail v
      pure $! head v

receive :: (Parser cmd, MonadIO m, Transport tp) => AgentT tp m (Either String cmd)
receive = runParser <$> receive_

readerSize :: MonadIO m => AgentT tp m Int
readerSize = fmap length $ readTVarIO =<< asks agentReader

data AgentEnv' tp = AgentEnv'
  { agentEnv :: AgentEnv
  , connEnv  :: ConnEnv tp
  }

agentid :: AgentEnv' tp -> ByteString
agentid AgentEnv'{..} = B.concat [agentMsgid agentEnv, connid' connEnv]

agentEnv' :: (Monad m, Transport tp) => AgentT tp m (AgentEnv' tp)
agentEnv' = do
  connEnv <- fromConn ask
  agentEnv <- ask
  pure AgentEnv'{..}

runAgentT' :: AgentEnv' tp -> AgentT tp m a -> m a
runAgentT' AgentEnv'{..} = runConnectionT connEnv . runAgentT agentEnv
