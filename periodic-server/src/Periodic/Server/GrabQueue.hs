{-# LANGUAGE TupleSections #-}

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

newtype GrabQueue = GrabQueue (TVar [(FuncName, (Nid, Msgid))])

newGrabQueue :: MonadIO m => m GrabQueue
newGrabQueue = GrabQueue <$> newTVarIO []

pushAgent :: MonadIO m => GrabQueue -> [FuncName] -> (Nid, Msgid) -> m ()
pushAgent (GrabQueue q) funcList agent = atomically $ do
  ov <- readTVar q
  writeTVar q $! nub $ ov ++ map (, agent) funcList

popAgentSTM :: GrabQueue -> FuncName -> STM (Nid, Msgid)
popAgentSTM (GrabQueue q) fn0 = do
  agents <- readTVar q
  case getOne agents of
    Nothing -> retrySTM
    Just agent -> do
      writeTVar q $! removeAgent agent agents
      return agent
  where getOne :: [(FuncName, (Nid, Msgid))] -> Maybe (Nid, Msgid)
        getOne [] = Nothing
        getOne ((fn1, agent) : xs)
          | fn1 == fn0 = Just agent
          | otherwise = getOne xs

        removeAgent :: (Nid, Msgid) -> [(FuncName, (Nid, Msgid))] -> [(FuncName, (Nid, Msgid))]
        removeAgent _ [] = []
        removeAgent agent0 ((fn1, agent1) : xs)
          | agent0 == agent1 = removeAgent agent0 xs
          | otherwise = (fn1, agent1) : removeAgent agent0 xs


popAgentList :: MonadIO m => GrabQueue -> FuncName -> m [(Nid, Msgid)]
popAgentList (GrabQueue q) fn0 = atomically $ do
  agents <- readTVar q
  let agentList = getAll agents
  writeTVar q $! removeAgent agentList agents
  return agentList
  where getAll :: [(FuncName, (Nid, Msgid))] -> [(Nid, Msgid)]
        getAll [] = []
        getAll ((fn1, agent) : xs)
          | fn1 == fn0 = agent : getAll xs
          | otherwise = getAll xs

        removeAgent :: [(Nid, Msgid)] -> [(FuncName, (Nid, Msgid))] -> [(FuncName, (Nid, Msgid))]
        removeAgent _ [] = []
        removeAgent agents ((fn1, agent1) : xs)
          | agent1 `elem` agents = removeAgent agents xs
          | otherwise = (fn1, agent1) : removeAgent agents xs
