{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Periodic.Node
  ( NodeEnv
  , NodeT
  , initEnv
  , withEnv
  , runNodeT
  , startMainLoop
  , startMainLoop_
  , withAgentT
  , isAlive
  , stopNodeT
  , env
  , newAgentEnv
  ) where

import           Control.Monad              (forever, mzero, void)
import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Maybe  (runMaybeT)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B (drop, empty, take)
import           Periodic.Agent             hiding (receive)
import           Periodic.Connection        (ConnectionT, FromConn (..), close,
                                             receive)
import           Periodic.IOHashMap         (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap         as HM (delete, elems, insert,
                                                   lookup, member)
import           Periodic.Transport         (Transport)
import           System.Entropy             (getEntropy)
import           System.Log.Logger          (errorM)
import           UnliftIO
import           UnliftIO.Concurrent        (forkIO)


data NodeEnv u = NodeEnv
  { uEnv       :: u
  , nodeStatus :: TVar Bool
  , agentList  :: IOHashMap Msgid AgentEnv
  }

newtype NodeT u tp m a = NodeT { unNodeT :: ReaderT (NodeEnv u) (ConnectionT tp m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (NodeEnv u)
    )

instance MonadUnliftIO m => MonadUnliftIO (NodeT u tp m) where
  askUnliftIO = NodeT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runNodeT r))
  withRunInIO inner = NodeT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runNodeT r)

instance MonadTrans (NodeT u tp) where
  lift = NodeT . lift . lift

instance FromConn (NodeT u) where
  fromConn = NodeT . lift

runNodeT :: NodeEnv u -> NodeT u tp m a -> ConnectionT tp m a
runNodeT nEnv = flip runReaderT nEnv . unNodeT

initEnv :: MonadIO m => u -> m (NodeEnv u)
initEnv uEnv = do
  nodeStatus <- newTVarIO True
  agentList <- newIOHashMap
  pure NodeEnv{..}

defaultAgentHandler :: MonadIO m => AgentT tp m ()
defaultAgentHandler = do
  pid <- msgid
  liftIO $ errorM "Node.Monad" $ "Agent [" ++ show pid ++ "] not found."

withEnv :: (Monad m) =>  u1 -> NodeT u1 tp m a -> NodeT u tp m a
withEnv u m = do
  env0 <- ask
  fromConn $ runNodeT (env0 {uEnv=u}) m

runAgentT_ :: Monad m => AgentEnv -> AgentT tp m a -> NodeT u tp m a
runAgentT_ aEnv = fromConn . runAgentT aEnv

withAgentT :: (MonadUnliftIO m) => AgentT tp m a -> NodeT u tp m a
withAgentT agentT =
  bracket newMsgid removeMsgid $ \mid -> do
    aEnv <- newAgentEnv_ mid
    runAgentT_ aEnv agentT

newAgentEnv_ :: (MonadIO m) => Msgid -> NodeT u tp m AgentEnv
newAgentEnv_ mid = do
  NodeEnv{..} <- ask
  reader <- mkAgentReader []
  HM.insert agentList mid $ AgentEnv reader mid
  return $ AgentEnv reader mid

newAgentEnv :: (MonadIO m) => NodeT u tp m AgentEnv
newAgentEnv = newAgentEnv_ =<< newMsgid

newMsgid :: MonadIO m => NodeT u tp m Msgid
newMsgid = do
  ref <- asks agentList
  aid <- liftIO $ getEntropy msgidLength
  has <- HM.member ref aid
  if has then newMsgid
         else pure aid

removeMsgid :: MonadIO m => Msgid -> NodeT u tp m ()
removeMsgid mid = do
  ref <- asks agentList
  HM.delete ref mid

tryMainLoop
  :: (MonadUnliftIO m, Transport tp)
  => AgentT tp m () -> NodeT u tp m ()
tryMainLoop agentHandler = do
  r <- tryAny $ mainLoop agentHandler
  case r of
    Left _  -> stopNodeT
    Right _ -> pure ()

mainLoop
  :: (MonadUnliftIO m, Transport tp)
  => AgentT tp m () -> NodeT u tp m ()
mainLoop agentHandler = do
  NodeEnv{..} <- ask
  bs <- fromConn receive
  void . forkIO $ tryDoFeed bs agentHandler

tryDoFeed :: (MonadUnliftIO m, Transport tp) => ByteString -> AgentT tp m () -> NodeT u tp m ()
tryDoFeed bs agentHandler = do
  r <- tryAny $ doFeed bs agentHandler
  case r of
    Left _  -> stopNodeT
    Right _ -> pure ()

doFeed :: MonadIO m => ByteString -> AgentT tp m () -> NodeT u tp m ()
doFeed bs agentHandler = do
  NodeEnv{..} <- ask
  v <- HM.lookup agentList $! B.take msgidLength bs
  case v of
    Just aEnv ->
      runAgentT_ aEnv . feed $! B.drop msgidLength bs
    Nothing    -> do
      let mid = B.take msgidLength bs
      reader <- mkAgentReader [B.drop msgidLength bs]
      runAgentT_ (AgentEnv reader mid) agentHandler

startMainLoop
  :: (MonadUnliftIO m, Transport tp)
  => NodeT u tp m ()
startMainLoop = startMainLoop_ defaultAgentHandler

startMainLoop_
  :: (MonadUnliftIO m, Transport tp)
  => AgentT tp m () -> NodeT u tp m ()
startMainLoop_ agentHandler = do
  void . runMaybeT . forever $ do
    alive <- lift isAlive
    if alive then lift $ tryMainLoop agentHandler
             else mzero

  doFeedError

isAlive :: MonadIO m => NodeT u tp m Bool
isAlive = readTVarIO =<< asks nodeStatus

doFeedError :: MonadIO m => NodeT u tp m ()
doFeedError =
  asks agentList >>= HM.elems >>= mapM_ go
  where go :: MonadIO m => AgentEnv -> NodeT u tp m ()
        go aEnv = runAgentT_ aEnv $ feed B.empty

stopNodeT :: (MonadIO m, Transport tp) => NodeT u tp m ()
stopNodeT = do
  st <- asks nodeStatus
  atomically $ writeTVar st False
  fromConn close

env :: Monad m => NodeT u tp m u
env = asks uEnv
