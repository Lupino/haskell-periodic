{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Periodic.Agent
  (
    AgentReader
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
  , liftC

  , AgentEnv'
  , agentEnv'
  , agentid
  , runAgentT'
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Base
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Reader.Class  (MonadReader (ask), asks)
import           Control.Monad.STM           (atomically, retry)
import           Control.Monad.Trans.Class   (MonadTrans (..))
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader  (ReaderT, runReaderT)
import           Data.Byteable               (Byteable (..))
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B (concat, empty)
import           Periodic.Connection         (ConnEnv, ConnectionT, connid',
                                              runConnectionT, statusTVar)
import qualified Periodic.Connection         as Conn (send)
import           Periodic.Types.Internal

type AgentReader = TVar [ByteString]
type Msgid = ByteString

data AgentEnv = AgentEnv
  { agentReader :: AgentReader
  , agentMsgid  :: Msgid
  }

newtype AgentT m a = AgentT { unAgentT :: ReaderT AgentEnv (ConnectionT m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AgentEnv)

instance MonadTrans AgentT where
  lift = AgentT . lift . lift

deriving instance MonadBase IO m => MonadBase IO (AgentT m)

instance MonadTransControl AgentT where
  type StT AgentT a = StT (ReaderT AgentEnv) (StT ConnectionT a)
  liftWith = defaultLiftWith2 AgentT unAgentT
  restoreT = defaultRestoreT2 AgentT

instance MonadBaseControl IO m => MonadBaseControl IO (AgentT m) where
  type StM (AgentT m) a = ComposeSt AgentT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

liftC :: Monad m => ConnectionT m a -> AgentT m a
liftC = AgentT . lift

runAgentT :: AgentEnv -> AgentT m a -> ConnectionT m a
runAgentT aEnv = flip runReaderT aEnv . unAgentT

mkAgentReader :: [ByteString] -> IO AgentReader
mkAgentReader = newTVarIO

msgid :: Monad m => AgentT m Msgid
msgid = asks agentMsgid

msgidLength :: Int
msgidLength = 4

aAlive :: MonadIO m => AgentT m Bool
aAlive = liftIO . readTVarIO =<< liftC statusTVar

send_ :: MonadIO m => ByteString -> AgentT m ()
send_ pl = do
  mid <- msgid
  liftC $ Conn.send $ B.concat [mid, pl]

send :: (Byteable cmd, MonadIO m) => cmd -> AgentT m ()
send = send_ . toBytes

feed :: (MonadIO m) => ByteString -> AgentT m ()
feed dat = do
  reader <- asks agentReader
  liftIO $ atomically . modifyTVar' reader $ \v -> v ++ [dat]

receive_ :: MonadIO m => AgentT m ByteString
receive_ = do
  reader <- asks agentReader
  st <- liftC statusTVar
  liftIO . atomically $ do
    v <- readTVar reader
    if null v then do
      s <- readTVar st
      if s then retry
           else pure B.empty
    else do
      writeTVar reader $ tail v
      pure $ head v

receive :: (Parser cmd, MonadIO m) => AgentT m (Either String cmd)
receive = runParser <$> receive_

readerSize :: MonadIO m => AgentT m Int
readerSize = fmap length $ liftIO . readTVarIO =<< asks agentReader

data AgentEnv' = AgentEnv'
  { agentEnv :: AgentEnv
  , connEnv  :: ConnEnv
  }

agentid :: AgentEnv' -> ByteString
agentid AgentEnv'{..} = B.concat [agentMsgid agentEnv, connid' connEnv]

agentEnv' :: Monad m => AgentT m AgentEnv'
agentEnv' = do
  connEnv <- liftC ask
  agentEnv <- ask
  pure AgentEnv'{..}

runAgentT' :: AgentEnv' -> AgentT m a -> m a
runAgentT' AgentEnv'{..} = runConnectionT connEnv . runAgentT agentEnv
