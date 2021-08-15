module Periodic.Server.GrabQueue
  ( GrabQueue
  , newGrabQueue
  , pushAgent
  , popAgent
  , popAgentList
  ) where


import           Data.IOMap     (IOMap)
import qualified Data.IOMap     as IOMap
import qualified Data.IOMap.STM as IOMapS
import           Data.Maybe     (catMaybes)
import           Periodic.Types (FuncName, Msgid, Nid)
import           UnliftIO       (MonadIO, STM, TVar, atomically, readTVar,
                                 retrySTM)

data GrabItem = GrabItem
  { msgidList :: ![Msgid]
  , funcList  :: TVar [FuncName]
  }

newtype GrabQueue = GrabQueue (IOMap Nid GrabItem)

newGrabQueue :: MonadIO m => m GrabQueue
newGrabQueue = GrabQueue <$> IOMap.empty

pushAgent :: MonadIO m => GrabQueue -> TVar [FuncName] -> Nid -> Msgid -> m ()
pushAgent (GrabQueue q) fl nid msgid =
  IOMap.alter alterFunc nid q
  where alterFunc :: Maybe GrabItem -> Maybe GrabItem
        alterFunc Nothing = Just $ GrabItem [msgid] fl
        alterFunc (Just item)
          | msgid `elem` msgidList item = Just item
          | otherwise = Just item {msgidList = msgidList item ++ [msgid]}

findMsgid :: GrabQueue -> FuncName -> Nid -> STM (Maybe (Nid, Msgid))
findMsgid (GrabQueue q) fn nid = do
  mitem <- IOMapS.lookup nid q
  case mitem of
    Nothing -> pure Nothing
    Just GrabItem{msgidList = []} -> pure Nothing
    Just item@GrabItem{msgidList=(x:xs)} -> do
      funcs <- readTVar $ funcList item
      if fn `elem` funcs then do
        IOMapS.insert nid item {msgidList = xs} q
        pure $ Just (nid, x)
        else pure Nothing

popAgent :: MonadIO m => GrabQueue -> FuncName -> m (Nid, Msgid)
popAgent gq@(GrabQueue q) fn = atomically $
  IOMapS.keys q
    >>= go
    >>= maybe retrySTM pure
  where go :: [Nid] -> STM (Maybe (Nid, Msgid))
        go [] = pure Nothing
        go (x:xs) = do
          r <- findMsgid gq fn x
          case r of
            Nothing -> go xs
            Just rr -> pure $ Just rr


popAgentList :: MonadIO m => GrabQueue -> FuncName -> m [(Nid, Msgid)]
popAgentList gq@(GrabQueue q) fn = atomically $ do
  nids <- IOMapS.keys q
  catMaybes <$> mapM (findMsgid gq fn) nids
