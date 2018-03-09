{-# LANGUAGE RecordWildCards #-}

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
  , agentid
  , msgid
  , msgidLength
  , aAlive
  , feed
  , receive
  , receive_
  , readerSize
  , agentEnv
  , runAgentTWithEnv
  , liftAgentT
  ) where

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B (concat)

import           Periodic.Connection         (ConnectionConfig, ConnectionState,
                                              ConnectionT, connected, connid',
                                              runConnectionT)
import qualified Periodic.Connection         as Conn (send)

import           Control.Concurrent.STM.TVar
import           Control.Monad               (when)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.STM           (atomically, retry)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.State   (StateT, evalStateT, get)
import           Data.Byteable               (Byteable (..))
import           Periodic.Types.Internal

type AgentReader = TVar [ByteString]
type Msgid = ByteString

data AgentEnv = AgentEnv
  { agentReader      :: AgentReader
  , agentMsgid       :: Msgid
  , connectionState  :: ConnectionState
  , connectionConfig :: ConnectionConfig
  }

agentid :: AgentEnv -> ByteString
agentid AgentEnv{..} = B.concat [agentMsgid, connid' connectionConfig]

type AgentT m = StateT AgentReader (ReaderT Msgid (ConnectionT m))

runAgentT
  :: Monad m
  => AgentReader
  -> Msgid
  -> AgentT m a
  -> ConnectionT m a
runAgentT reader mid =
  flip runReaderT mid
    . flip evalStateT reader

runAgentTWithEnv :: Monad m => AgentEnv -> AgentT m a -> m a
runAgentTWithEnv AgentEnv {..} =
  runConnectionT connectionState connectionConfig
    . runAgentT agentReader agentMsgid

mkAgentReader :: [ByteString] -> IO AgentReader
mkAgentReader = newTVarIO

msgid :: Monad m => AgentT m Msgid
msgid = lift ask

msgidLength :: Int
msgidLength = 4

aAlive :: MonadIO m => AgentT m Bool
aAlive = lift . lift $ connected

send_ :: MonadIO m => ByteString -> AgentT m ()
send_ pl = do
  mid <- msgid
  lift . lift $ Conn.send $ B.concat [mid, pl]

send :: (Byteable cmd, MonadIO m) => cmd -> AgentT m ()
send = send_ . toBytes

feed :: (MonadIO m) => ByteString -> AgentT m ()
feed dat = do
  state <- get
  liftIO $ atomically . modifyTVar' state $ \v -> v ++ [dat]

receive_ :: MonadIO m => AgentT m ByteString
receive_ = do
  state <- get
  liftIO . atomically $ do
    v <- readTVar state
    when (null v) retry
    writeTVar state $ tail v
    pure $ head v

receive :: (Parser cmd, MonadIO m) => AgentT m (Either String cmd)
receive = runParser <$> receive_

readerSize :: MonadIO m => AgentT m Int
readerSize = fmap length $ liftIO . readTVarIO =<< get

agentEnv :: Monad m => AgentT m AgentEnv
agentEnv = do
  agentReader <- get
  agentMsgid <- lift ask
  connectionState <- lift $ lift get
  connectionConfig <- lift . lift $ lift ask
  pure AgentEnv{..}

liftAgentT :: Monad m => m a -> AgentT m a
liftAgentT = lift . lift . lift . lift
