module Periodic.Server.GrabQueue
  ( GrabQueue
  , newGrabQueue
  , pushAgent
  , popAgent
  -- , popAgentList
  ) where


import           Control.Monad  (unless)
import           Periodic.Types (FuncName, Msgid, Nid)
import           UnliftIO       (MonadIO, STM, TVar, atomically, newTVarIO,
                                 readTVar, retrySTM, writeTVar)

data GrabItem = GrabItem
  { gNid      :: !Nid
  , gMsgid    :: !Msgid
  , gFuncList :: TVar [FuncName]
  }

instance Eq GrabItem where
  a == b = gNid a == gNid b && gMsgid a == gMsgid b

newtype GrabQueue = GrabQueue (TVar [GrabItem])

newGrabQueue :: MonadIO m => m GrabQueue
newGrabQueue = GrabQueue <$> newTVarIO []

pushAgent :: MonadIO m => GrabQueue -> TVar [FuncName] -> Nid -> Msgid -> m ()
pushAgent (GrabQueue q) funcList nid msgid = atomically $ do
  ov <- readTVar q
  unless (item `elem` ov) $ writeTVar q $! ov ++ [item]
  where item = GrabItem nid msgid funcList

popAgent :: MonadIO m => GrabQueue -> FuncName -> m (Nid, Msgid)
popAgent (GrabQueue q) fn = atomically $ do
  (mmsgid, items) <- lookupAndRemove [] =<< readTVar q
  case mmsgid of
    Nothing -> retrySTM
    Just item -> do
      writeTVar q $! items
      return (gNid item, gMsgid item)

  where lookupAndRemove :: [GrabItem] -> [GrabItem] -> STM (Maybe GrabItem, [GrabItem])
        lookupAndRemove prev [] = pure (Nothing, prev)
        lookupAndRemove prev (x:xs) = do
          funcList <- readTVar $ gFuncList x
          if fn `elem` funcList then pure (Just x, prev ++ xs)
                                else lookupAndRemove (prev ++ [x]) xs
