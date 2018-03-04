{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Periodic.Monad
  (
    Env
  , PeriodicState
  , PeriodicT
  , initEnv
  , initPeriodicState
  , runPeriodicT
  , startMainLoop
  , withAgentT
  , liftPeriodicT
  , isAlive
  , stopPeriodicT
  ) where

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.STM.TVar
import           Control.Exception           (SomeException)
import           Control.Monad               (forever, mzero, unless, void)
import           Control.Monad.Catch         (MonadCatch, MonadMask, bracket,
                                              try)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseDiscard)
import           Control.Monad.Trans.Maybe   (runMaybeT)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State   (StateT (..), evalStateT, get,
                                              gets)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B (drop, empty, take)
import           Data.Typeable               (Typeable)
import           Periodic.Agent              hiding (receive)
import           Periodic.Connection         (ConnectionConfig, ConnectionState,
                                              ConnectionT, close, receive,
                                              runConnectionT)
import           Periodic.IOHashMap          (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap          as HM (delete, elems, insert,
                                                    lookup, member)
import           Periodic.Types
import           System.Entropy              (getEntropy)
import           System.Log.Logger           (errorM)


data Env m u = Env
  { uEnv             :: u
  , connectionConfig :: ConnectionConfig
  , agentHandler     :: AgentT m ()
  }

data PeriodicState = PeriodicState
  { status          :: TVar Bool
  , agentList       :: AgentList
  , connectionState :: ConnectionState
  }

type PeriodicT m u = StateT PeriodicState (ReaderT (Env m u) (ConnectionT m))

runPeriodicT
  :: Monad m
  => PeriodicState
  -> Env m u
  -> PeriodicT m u a
  -> m a
runPeriodicT state config =
  runConnectionT (connectionState state) (connectionConfig config)
    . flip runReaderT config
    . flip evalStateT state

initEnv :: u -> ConnectionConfig -> AgentT m () -> Env m u
initEnv = Env

initPeriodicState :: ConnectionState -> IO PeriodicState
initPeriodicState connectionState = do
  status <- newTVarIO True
  agentList <- newIOHashMap
  pure $ PeriodicState{..}

withAgentT :: (MonadIO m, Monad m, MonadMask m) => AgentT m a -> PeriodicT m u a
withAgentT agentT = do
  PeriodicState{..} <- get
  Env {..} <- lift $ ask
  bracket newMsgid removeMsgid $ \mid -> do
    let agentConfig = initAgentConfig mid connectionConfig
    agentState <- liftIO $ initAgentState connectionState
    liftIO $ HM.insert agentList mid (agentState, agentConfig)
    liftPeriodicT $ runAgentT agentState agentConfig agentT

liftPeriodicT :: (Functor m, Applicative m, Monad m) => m a -> PeriodicT m u a
liftPeriodicT m =
  StateT $ \s0 ->
    ReaderT $ \_ -> do
      a0 <- StateT $ \s1 ->
        ReaderT $ \_ -> do
          a1 <- m
          pure (a1, s1)
      pure (a0, s0)

newMsgid :: MonadIO m => PeriodicT m u Msgid
newMsgid = do
  ref <- gets agentList
  aid <- liftIO $ getEntropy msgidLength
  has <- liftIO $ HM.member ref aid
  if has then newMsgid
         else pure aid

removeMsgid :: MonadIO m => Msgid -> PeriodicT m u ()
removeMsgid mid = do
  ref <- gets agentList
  liftIO $ HM.delete ref mid

tryMainLoop :: (MonadIO m, MonadBaseControl IO m, MonadCatch m) => PeriodicT m u ()
tryMainLoop = do
  r <- try mainLoop
  case r of
    Left (_::SomeException) -> stopPeriodicT
    Right _                 -> pure ()

mainLoop
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => PeriodicT m u ()
mainLoop = do
  PeriodicState{..} <- get
  Env{..} <- lift $ ask
  bs <- lift . lift $ receive
  void . liftBaseDiscard forkIO $ tryDoFeed bs

tryDoFeed :: (MonadIO m, MonadCatch m) => ByteString -> PeriodicT m u ()
tryDoFeed bs = do
  r <- try $ doFeed bs
  case r of
    Left (_::SomeException) -> stopPeriodicT
    Right _                 -> pure ()

doFeed :: MonadIO m => ByteString -> PeriodicT m u ()
doFeed bs = do
  PeriodicState{..} <- get
  Env{..} <- lift $ ask
  v <- liftIO . HM.lookup agentList $ B.take msgidLength bs
  case v of
    Just (agentState, agentConfig) ->
      liftPeriodicT . runAgentT agentState agentConfig . feed $ B.drop msgidLength bs
    Nothing    -> do
      let agentConfig = initAgentConfig (B.take msgidLength bs) connectionConfig
      agentState <- liftIO $ initAgentState connectionState
      liftPeriodicT . runAgentT agentState agentConfig $ do
        feed $ B.drop msgidLength bs
        agentHandler

startMainLoop
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => IO () -> PeriodicT m u ()
startMainLoop onClose = do
  void . runMaybeT . forever $ do
    alive <- lift isAlive
    if alive then lift tryMainLoop
             else mzero

  doFeedError
  lift $ lift close
  liftIO $ onClose

isAlive :: MonadIO m => PeriodicT m u Bool
isAlive = liftIO . readTVarIO =<< gets status

doFeedError :: MonadIO m => PeriodicT m u ()
doFeedError = do
  gets agentList >>= liftIO . HM.elems >>= mapM_ go
  where go :: MonadIO m => (AgentState, AgentConfig) -> PeriodicT m u ()
        go (agentState, agentConfig) =
          liftPeriodicT $ runAgentT agentState agentConfig $ feed B.empty

stopPeriodicT :: MonadIO m => PeriodicT m u ()
stopPeriodicT = do
  st <- gets status
  liftIO . atomically $ writeTVar st False

env :: Monad m => PeriodicT m u u
env = lift $ asks uEnv
