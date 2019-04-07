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

newtype AgentT m a = AgentT { unAgentT :: ReaderT AgentEnv (ConnectionT m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AgentEnv)

instance MonadTrans AgentT where
  lift = AgentT . lift . lift

instance MonadUnliftIO m => MonadUnliftIO (AgentT m) where
  askUnliftIO = AgentT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runAgentT r))
  withRunInIO inner = AgentT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runAgentT r)

instance Monad m => FromConn m AgentT where
  fromConn = AgentT . lift

runAgentT :: AgentEnv -> AgentT m a -> ConnectionT m a
runAgentT aEnv = flip runReaderT aEnv . unAgentT

mkAgentReader :: MonadIO m => [ByteString] -> m AgentReader
mkAgentReader = newTVarIO

msgid :: Monad m => AgentT m Msgid
msgid = asks agentMsgid

msgidLength :: Int
msgidLength = 4

aAlive :: MonadIO m => AgentT m Bool
aAlive = readTVarIO =<< fromConn statusTVar

send_ :: MonadUnliftIO m => ByteString -> AgentT m ()
send_ pl = do
  mid <- msgid
  fromConn $ Conn.send $ B.concat [mid, pl]

send :: (Byteable cmd, Validatable cmd, MonadUnliftIO m) => cmd -> AgentT m ()
send cmd =
  case validate cmd of
    Right _ -> send_ $ toBytes cmd
    Left e  -> do
      liftIO $ errorM "Periodic.Agent" $ "InValidError " ++ e
      throwIO $ InValidError e

feed :: (MonadIO m) => ByteString -> AgentT m ()
feed dat = do
  reader <- asks agentReader
  atomically . modifyTVar' reader $ \v -> v ++ [dat]

receive_ :: MonadIO m => AgentT m ByteString
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

receive :: (Parser cmd, MonadIO m) => AgentT m (Either String cmd)
receive = runParser <$> receive_

readerSize :: MonadIO m => AgentT m Int
readerSize = fmap length $ readTVarIO =<< asks agentReader

data AgentEnv' = AgentEnv'
  { agentEnv :: AgentEnv
  , connEnv  :: ConnEnv
  }

agentid :: AgentEnv' -> ByteString
agentid AgentEnv'{..} = B.concat [agentMsgid agentEnv, connid' connEnv]

agentEnv' :: Monad m => AgentT m AgentEnv'
agentEnv' = do
  connEnv <- fromConn ask
  agentEnv <- ask
  pure AgentEnv'{..}

runAgentT' :: AgentEnv' -> AgentT m a -> m a
runAgentT' AgentEnv'{..} = runConnectionT connEnv . runAgentT agentEnv
