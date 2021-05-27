module Periodic.Server.GrabQueue
  ( GrabQueue
  , newGrabQueue
  , pushAgent
  , popAgentSTM
  , popAgentList
  ) where


import           Data.List      (nub)
import           Periodic.Types (FuncName, Msgid, Nid)
import           UnliftIO       (MonadIO, STM, TVar, atomically, newTVarIO,
                                 readTVar, retrySTM, writeTVar)

data GrabItem = GrabItem
  { gNid      :: Nid
  , gMsgid    :: Msgid
  , gFuncList :: [FuncName]
  }

instance Eq GrabItem where
  a == b = gNid a == gNid b && gMsgid a == gMsgid b

newtype GrabQueue = GrabQueue (TVar [GrabItem])

newGrabQueue :: MonadIO m => m GrabQueue
newGrabQueue = GrabQueue <$> newTVarIO []

pushAgent :: MonadIO m => GrabQueue -> [FuncName] -> (Nid, Msgid) -> m ()
pushAgent (GrabQueue q) funcList (nid, msgid) = atomically $ do
  ov <- readTVar q
  writeTVar q $! nub $ item : ov
  where item = GrabItem nid msgid funcList

popAgentSTM :: GrabQueue -> FuncName -> STM (Nid, Msgid)
popAgentSTM (GrabQueue q) fn0 = do
  (magent, items) <- lookupAndRemove [] <$> readTVar q
  case magent of
    Nothing -> retrySTM
    Just agent -> do
      writeTVar q $! items
      return agent

  where lookupAndRemove :: [GrabItem] -> [GrabItem] -> (Maybe (Nid, Msgid), [GrabItem])
        lookupAndRemove prev [] = (Nothing, prev)
        lookupAndRemove prev (x:xs)
          | fn0 `elem` gFuncList x = (Just (gNid x, gMsgid x), prev ++ xs)
          | otherwise = lookupAndRemove (prev ++ [x]) xs


popAgentList :: MonadIO m => GrabQueue -> FuncName -> m [(Nid, Msgid)]
popAgentList (GrabQueue q) fn0 = atomically $ do
  (agents, items) <- lookupAndRemove [] [] <$> readTVar q
  writeTVar q $! items
  return agents
  where lookupAndRemove :: [(Nid, Msgid)] -> [GrabItem] -> [GrabItem] -> ([(Nid, Msgid)], [GrabItem])
        lookupAndRemove agents prev [] = (agents, prev)
        lookupAndRemove agents prev (x:xs)
          | fn0 `elem` gFuncList x = lookupAndRemove (agents ++ [(gNid x, gMsgid x)]) prev xs
          | otherwise = lookupAndRemove agents (prev ++ [x]) xs
