{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
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
  , liftC
  ) where

import           Control.Exception          (SomeException)
import           Control.Monad              (forever, mzero, void)
import           Control.Monad.Reader.Class (MonadReader (ask), asks)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Trans.Maybe  (runMaybeT)
import           Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B (drop, empty, take)
import           Periodic.Agent             hiding (liftC, receive)
import           Periodic.Connection        (ConnectionT, close, receive)
import           Periodic.IOHashMap         (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap         as HM (delete, elems, insert,
                                                   lookup, member)
import           System.Entropy             (getEntropy)
import           System.Log.Logger          (errorM)
import           UnliftIO
import           UnliftIO.Concurrent        (forkIO)


data NodeEnv u = NodeEnv
  { uEnv       :: u
  , nodeStatus :: TVar Bool
  , agentList  :: IOHashMap Msgid AgentEnv
  }

newtype NodeT u m a = NodeT { unNodeT :: ReaderT (NodeEnv u) (ConnectionT m) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (NodeEnv u)
    )

instance MonadUnliftIO m => MonadUnliftIO (NodeT u m) where
  askUnliftIO = NodeT $
    ReaderT $ \r ->
      withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . runNodeT r))
  withRunInIO inner = NodeT $
    ReaderT $ \r ->
      withRunInIO $ \run ->
        inner (run . runNodeT r)

instance MonadTrans (NodeT u) where
  lift = NodeT . lift . lift

liftC :: Monad m => ConnectionT m a -> NodeT u m a
liftC = NodeT . lift

runNodeT :: NodeEnv u -> NodeT u m a -> ConnectionT m a
runNodeT nEnv = flip runReaderT nEnv . unNodeT

initEnv :: u -> IO (NodeEnv u)
initEnv uEnv = do
  nodeStatus <- newTVarIO True
  agentList <- newIOHashMap
  pure NodeEnv{..}

defaultAgentHandler :: MonadIO m => AgentT m ()
defaultAgentHandler = do
  pid <- msgid
  liftIO $ errorM "Node.Monad" $ "Agent [" ++ show pid ++ "] not found."

withEnv :: (Monad m) =>  u1 -> NodeT u1 m a -> NodeT u m a
withEnv u m = do
  env0 <- ask
  liftC $ runNodeT (env0 {uEnv=u}) m

withAgentT :: (MonadUnliftIO m) => AgentT m a -> NodeT u m a
withAgentT agentT =
  bracket newMsgid removeMsgid $ \mid -> do
    aEnv <- newAgentEnv_ mid
    liftC $ runAgentT aEnv agentT

newAgentEnv_ :: (MonadIO m) => Msgid -> NodeT u m AgentEnv
newAgentEnv_ mid = do
  NodeEnv{..} <- ask
  reader <- mkAgentReader []
  HM.insert agentList mid $ AgentEnv reader mid
  return $ AgentEnv reader mid

newAgentEnv :: (MonadIO m) => NodeT u m AgentEnv
newAgentEnv = newAgentEnv_ =<< newMsgid

newMsgid :: MonadIO m => NodeT u m Msgid
newMsgid = do
  ref <- asks agentList
  aid <- liftIO $ getEntropy msgidLength
  has <- HM.member ref aid
  if has then newMsgid
         else pure aid

removeMsgid :: MonadIO m => Msgid -> NodeT u m ()
removeMsgid mid = do
  ref <- asks agentList
  HM.delete ref mid

tryMainLoop
  :: (MonadUnliftIO m)
  => AgentT m () -> NodeT u m ()
tryMainLoop agentHandler = do
  r <- try $ mainLoop agentHandler
  case r of
    Left (_::SomeException) -> stopNodeT
    Right _                 -> pure ()

mainLoop
  :: (MonadUnliftIO m)
  => AgentT m () -> NodeT u m ()
mainLoop agentHandler = do
  NodeEnv{..} <- ask
  bs <- liftC receive
  void . forkIO $ tryDoFeed bs agentHandler

tryDoFeed :: (MonadUnliftIO m) => ByteString -> AgentT m () -> NodeT u m ()
tryDoFeed bs agentHandler = do
  r <- try $ doFeed bs agentHandler
  case r of
    Left (_::SomeException) -> stopNodeT
    Right _                 -> pure ()

doFeed :: MonadIO m => ByteString -> AgentT m () -> NodeT u m ()
doFeed bs agentHandler = do
  NodeEnv{..} <- ask
  v <- HM.lookup agentList $! B.take msgidLength bs
  case v of
    Just aEnv ->
      liftC . runAgentT aEnv . feed $! B.drop msgidLength bs
    Nothing    -> do
      let mid = B.take msgidLength bs
      reader <- mkAgentReader [B.drop msgidLength bs]
      liftC $ runAgentT (AgentEnv reader mid) agentHandler

startMainLoop
  :: (MonadUnliftIO m)
  => NodeT u m ()
startMainLoop = startMainLoop_ defaultAgentHandler

startMainLoop_
  :: (MonadUnliftIO m)
  => AgentT m () -> NodeT u m ()
startMainLoop_ agentHandler = do
  void . runMaybeT . forever $ do
    alive <- lift isAlive
    if alive then lift $ tryMainLoop agentHandler
             else mzero

  doFeedError

isAlive :: MonadIO m => NodeT u m Bool
isAlive = readTVarIO =<< asks nodeStatus

doFeedError :: MonadIO m => NodeT u m ()
doFeedError =
  asks agentList >>= HM.elems >>= mapM_ go
  where go :: MonadIO m => AgentEnv -> NodeT u m ()
        go aEnv =
          liftC . runAgentT aEnv $ feed B.empty

stopNodeT :: MonadIO m => NodeT u m ()
stopNodeT = do
  st <- asks nodeStatus
  atomically $ writeTVar st False
  liftC close

env :: Monad m => NodeT u m u
env = asks uEnv
