module Periodic.Server.GrabQueue
  ( GrabQueue
  , newGrabQueue
  , pushAgent
  , popAgent
  , popAgentList
  , dropAgentList
  ) where


import           Data.Foldable  (toList)
import           Data.IOMap     (IOMap)
import qualified Data.IOMap     as IOMap
import qualified Data.IOMap.STM as IOMapS
import           Data.List      (delete)
import           Data.Maybe     (catMaybes)
import           Periodic.Types (FuncName, Msgid, Nid)
import           UnliftIO       (MonadIO, STM, TVar, atomically, newTVarIO,
                                 readTVar, retrySTM, writeTVar)

data GrabItem = GrabItem
  { msgidList :: TVar [Msgid]
  , funcList  :: TVar [FuncName]
  }


data GrabQueue = GrabQueue (TVar [Nid]) (IOMap Nid GrabItem)

newGrabQueue :: MonadIO m => m GrabQueue
newGrabQueue = do
  nidList <- newTVarIO []
  GrabQueue nidList <$> IOMap.empty

pushAgent :: MonadIO m => GrabQueue -> TVar [FuncName] -> Nid -> TVar [Msgid] -> m ()
pushAgent (GrabQueue s q) fl nid ml = atomically $ do
  nl <- readTVar s
  writeTVar s $! nid : nl
  IOMapS.insert nid (GrabItem ml fl) q

findMsgid :: GrabQueue -> FuncName -> Nid -> STM (Maybe (Nid, Msgid))
findMsgid (GrabQueue _ q) fn nid = do
  mitem <- IOMapS.lookup nid q
  case mitem of
    Nothing -> pure Nothing
    Just (GrabItem ml fl) -> do
      msgids <- readTVar ml
      case msgids of
        []     -> pure Nothing
        (x:xs) -> do
          funcs <- readTVar fl
          if fn `elem` funcs then do
                             writeTVar ml xs
                             pure $ Just (nid, x)
                             else pure Nothing


popAgent :: MonadIO m => GrabQueue -> FuncName -> m (Nid, Msgid)
popAgent gq@(GrabQueue s _) fn = atomically $
  readTVar s
    >>= go []
    >>= maybe retrySTM pure
  where go :: [Nid] -> [Nid] -> STM (Maybe (Nid, Msgid))
        go _ [] = pure Nothing
        go ys (x:xs) = do
          r <- findMsgid gq fn x
          case r of
            Nothing -> go (ys ++ [x]) xs
            Just (nid, msgid) -> do
              writeTVar s $! xs ++ ys ++ [x]
              pure $ Just (nid, msgid)


popAgentList :: MonadIO m => GrabQueue -> FuncName -> m [(Nid, Msgid)]
popAgentList gq@(GrabQueue s _) fn = atomically $ do
  seqV <- toList <$> readTVar s
  catMaybes <$> mapM (findMsgid gq fn) seqV

dropAgentList :: MonadIO m => GrabQueue -> Nid -> m ()
dropAgentList (GrabQueue s q) nid = atomically $ do
  IOMapS.delete nid q
  nids <- readTVar s
  writeTVar s $! delete nid nids
