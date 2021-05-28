module Periodic.Server.GrabQueue
  ( GrabQueue
  , newGrabQueue
  , pushAgent
  , popAgentSTM
  -- , popAgentList
  ) where


import           Control.Monad  (unless)
import           Periodic.Types (Msgid, Nid)
import           UnliftIO       (MonadIO, STM, TVar, atomically, newTVarIO,
                                 readTVar, writeTVar)

data GrabItem = GrabItem
  { gNid   :: !Nid
  , gMsgid :: !Msgid
  }
  deriving (Eq, Show)

newtype GrabQueue = GrabQueue (TVar [GrabItem])

newGrabQueue :: MonadIO m => m GrabQueue
newGrabQueue = GrabQueue <$> newTVarIO []

pushAgent :: MonadIO m => GrabQueue -> Nid -> Msgid -> m ()
pushAgent (GrabQueue q) nid msgid = atomically $ do
  ov <- readTVar q
  unless (item `elem` ov) $ writeTVar q $! ov ++ [item]
  where item = GrabItem nid msgid

popAgentSTM :: GrabQueue -> Nid -> STM (Maybe Msgid)
popAgentSTM (GrabQueue q) nid = do
  (mmsgid, items) <- lookupAndRemove [] <$> readTVar q
  writeTVar q $! items
  return mmsgid

  where lookupAndRemove :: [GrabItem] -> [GrabItem] -> (Maybe Msgid, [GrabItem])
        lookupAndRemove prev [] = (Nothing, prev)
        lookupAndRemove prev (x:xs)
          | nid == gNid x = (Just (gMsgid x), prev ++ xs)
          | otherwise = lookupAndRemove (prev ++ [x]) xs
