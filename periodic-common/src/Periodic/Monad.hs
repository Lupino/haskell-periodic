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
  , initEnv_
  , withEnv
  , initPeriodicState
  , runPeriodicT
  , startMainLoop
  , withAgentT
  , liftPeriodicT
  , isAlive
  , stopPeriodicT
  , env
  , newAgentEnv
  ) where

import           Control.Concurrent          (forkIO)
import           Control.Concurrent.STM.TVar
import           Control.Exception           (SomeException)
import           Control.Monad               (forever, mzero, void)
import           Control.Monad.Catch         (MonadCatch, MonadMask, bracket,
                                              try)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseDiscard)
import           Control.Monad.Trans.Maybe   (runMaybeT)
import           Control.Monad.Trans.Reader  (ReaderT, ask, asks, runReaderT)
import           Control.Monad.Trans.State   (StateT (..), evalStateT, get,
                                              gets)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B (drop, empty, take)
import           Periodic.Agent              hiding (receive)
import           Periodic.Connection         (ConnectionT, close, receive)
import           Periodic.IOHashMap          (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap          as HM (delete, elems, insert,
                                                    lookup, member)
import           System.Entropy              (getEntropy)
import           System.Log.Logger           (errorM)


data Env m u = Env
  { uEnv         :: u
  , agentHandler :: AgentT m ()
  }

data PeriodicState = PeriodicState
  { status    :: TVar Bool
  , agentList :: IOHashMap Msgid (Msgid, AgentReader)
  }

type PeriodicT m u = StateT PeriodicState (ReaderT (Env m u) (ConnectionT m))

runPeriodicT
  :: Monad m
  => PeriodicState
  -> Env m u
  -> PeriodicT m u a
  -> ConnectionT m a
runPeriodicT state config =
  flip runReaderT config
    . flip evalStateT state

initEnv :: MonadIO m => u -> Env m u
initEnv u = Env u defaultAgentHandler

initEnv_ :: u -> AgentT m () -> Env m u
initEnv_ = Env

defaultAgentHandler :: MonadIO m => AgentT m ()
defaultAgentHandler = do
  pid <- msgid
  liftIO $ errorM "Periodic.Monad" $ "Agent [" ++ show pid ++ "] not found."

withEnv :: (Monad m) =>  u1 -> PeriodicT m u1 a -> PeriodicT m u a
withEnv u m = do
  state0 <- get
  env0 <- lift ask
  lift . lift $ runPeriodicT state0 (env0 {uEnv=u}) m

initPeriodicState :: IO PeriodicState
initPeriodicState = do
  status <- newTVarIO True
  agentList <- newIOHashMap
  pure PeriodicState{..}

withAgentT :: (MonadIO m, Monad m, MonadMask m) => AgentT m a -> PeriodicT m u a
withAgentT agentT =
  bracket newMsgid removeMsgid $ \mid -> do
    (_, reader) <- newAgentEnv_ mid
    lift . lift $ runAgentT reader mid agentT

newAgentEnv_ :: (Monad m, MonadIO m) => Msgid -> PeriodicT m u (Msgid, AgentReader)
newAgentEnv_ mid = do
  PeriodicState{..} <- get
  reader <- liftIO $ mkAgentReader []
  liftIO $ HM.insert agentList mid (mid, reader)
  return (mid, reader)

newAgentEnv :: (MonadIO m) => PeriodicT m u (Msgid, AgentReader)
newAgentEnv = newAgentEnv_ =<< newMsgid

liftPeriodicT :: (Functor m, Applicative m, Monad m) => m a -> PeriodicT m u a
liftPeriodicT = lift . lift . lift . lift

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
  Env{..} <- lift ask
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
  Env{..} <- lift ask
  v <- liftIO . HM.lookup agentList $ B.take msgidLength bs
  case v of
    Just (mid, reader) ->
      lift . lift . runAgentT reader mid . feed $ B.drop msgidLength bs
    Nothing    -> do
      let mid = B.take msgidLength bs
      reader <- liftIO $ mkAgentReader [B.drop msgidLength bs]
      lift . lift $ runAgentT reader mid agentHandler

startMainLoop
  :: (MonadIO m, MonadBaseControl IO m, MonadCatch m)
  => PeriodicT m u ()
startMainLoop = do
  void . runMaybeT . forever $ do
    alive <- lift isAlive
    if alive then lift tryMainLoop
             else mzero

  doFeedError
  lift $ lift close

isAlive :: MonadIO m => PeriodicT m u Bool
isAlive = liftIO . readTVarIO =<< gets status

doFeedError :: MonadIO m => PeriodicT m u ()
doFeedError =
  gets agentList >>= liftIO . HM.elems >>= mapM_ go
  where go :: MonadIO m => (Msgid, AgentReader) -> PeriodicT m u ()
        go (mid, reader) =
          lift . lift $ runAgentT reader mid $ feed B.empty

stopPeriodicT :: MonadIO m => PeriodicT m u ()
stopPeriodicT = do
  st <- gets status
  liftIO . atomically $ writeTVar st False

env :: Monad m => PeriodicT m u u
env = lift $ asks uEnv
