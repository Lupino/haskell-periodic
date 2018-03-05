{-# LANGUAGE RecordWildCards #-}

module Periodic.Agent
  (
    AgentState
  , AgentConfig
  , AgentList
  , Agent
  , Msgid
  , AgentT
  , runAgentT
  , initAgentConfig
  , initAgentState
  , send
  , send_
  , agentid
  , msgid
  , msgid'
  , msgidLength
  , aAlive
  , feed
  , receive
  , receive_
  , readerSize
  , agent
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
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State   (StateT, evalStateT, get)
import           Data.Byteable               (Byteable (..))
import           Periodic.IOHashMap          (IOHashMap)
import           Periodic.Types.Internal

type AgentReader = TVar [ByteString]
type Msgid = ByteString

data AgentState = AgentState
  { aReader         :: AgentReader
  , connectionState :: ConnectionState
  }

data AgentConfig = AgentConfig
  { aMsgid           :: Msgid
  , connectionConfig :: ConnectionConfig
  }

type Agent = (AgentState, AgentConfig)
type AgentList = IOHashMap Msgid Agent

msgid' :: AgentConfig -> Msgid
msgid' = aMsgid

agentid :: AgentConfig -> ByteString
agentid config = B.concat [aMsgid config, connid' $ connectionConfig config]

type AgentT m = StateT AgentReader (ReaderT Msgid (ConnectionT m))

runAgentT
  :: Monad m
  => AgentState
  -> AgentConfig
  -> AgentT m a
  -> m a
runAgentT state config =
  runConnectionT (connectionState state) (connectionConfig config)
    . flip runReaderT (aMsgid config)
    . flip evalStateT (aReader state)

initAgentState :: ConnectionState -> IO AgentState
initAgentState connectionState = do
  aReader <- newTVarIO []
  return AgentState{..}

initAgentConfig :: Msgid -> ConnectionConfig -> AgentConfig
initAgentConfig = AgentConfig

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

agent :: Monad m => AgentT m Agent
agent = do
  aReader <- get
  aMsgid <- lift ask
  connectionState <- lift $ lift get
  connectionConfig <- lift . lift $ lift ask
  pure (AgentState {..}, AgentConfig {..})
