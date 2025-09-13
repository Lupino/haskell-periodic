module Periodic.Server.GrabQueue
  ( GrabQueue
  , newGrabQueue
  , pushAgent
  , popAgentSTM
  , popAgentListSTM
  , dropAgentList
  ) where


import           Control.Monad  (when)
import           Data.IOMap     (IOMap)
import qualified Data.IOMap     as IOMap
import qualified Data.IOMap.STM as IOMapS
import           Data.Maybe     (catMaybes)
import           Metro.Utils    (foreverExit, lift)
import           Periodic.Types (FuncName, Msgid, Nid)
import           UnliftIO       (MonadIO, STM, TQueue, TVar, atomically,
                                 newTQueueIO, readTQueue, readTVar, unGetTQueue,
                                 writeTQueue, writeTVar)

data GrabItem = GrabItem
  { msgidList :: TVar [Msgid]
  , funcList  :: TVar [FuncName]
  }


data GrabQueue = GrabQueue (TQueue Nid) (IOMap Nid GrabItem)

newGrabQueue :: MonadIO m => m GrabQueue
newGrabQueue = do
  nidList <- newTQueueIO
  GrabQueue nidList <$> IOMap.empty

pushAgent :: MonadIO m => GrabQueue -> TVar [FuncName] -> Nid -> TVar [Msgid] -> m ()
pushAgent (GrabQueue s q) fl nid ml = atomically $ do
  writeTQueue s nid
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


popAgentSTM :: GrabQueue -> FuncName -> STM (Maybe (Nid, Msgid))
popAgentSTM gq@(GrabQueue s _) fn = do
  first <- readTQueue s
  writeTQueue s first
  foreverExit $ \exit -> do
    nid <- lift $ readTQueue s
    r <- lift $ findMsgid gq fn nid
    case r of
      Nothing -> do
        lift $ writeTQueue s nid
        when (nid == first) $ exit Nothing
      Just (_, msgid) -> do
        lift $ unGetTQueue s nid
        exit $ Just (nid, msgid)


popAgentListSTM :: GrabQueue -> FuncName -> STM [(Nid, Msgid)]
popAgentListSTM gq@(GrabQueue _ s) fn = do
  seqV <- IOMapS.keys s
  catMaybes <$> mapM (findMsgid gq fn) seqV

dropAgentList :: MonadIO m => GrabQueue -> Nid -> m ()
dropAgentList (GrabQueue s q) nid = atomically $ do
  IOMapS.delete nid q
  first <- readTQueue s
  writeTQueue s first
  foreverExit $ \exit -> do
    nid0 <- lift $ readTQueue s
    when (nid0 /= nid) $ lift $ writeTQueue s nid0
    when (nid0 == first) $ exit ()
