{-# LANGUAGE RecordWildCards #-}

module Periodic.Server.GrabQueue
  ( GrabQueue
  , newGrabQueue
  , GrabItem (..)
  , pushAgent
  , popAgentSTM
  , popAgentList
  ) where


import           Data.IOMap            (IOMap)
import qualified Data.IOMap            as IOMap
import qualified Data.IOMap.STM        as IOMapS
import           Data.List             (nub)
import           Data.Maybe            (catMaybes)
import           Metro.Session         (ident)
import           Periodic.Node         (Nid)
import           Periodic.Server.Types (CSEnv)
import           Periodic.Types        (FuncName, JobHandle, Msgid)
import           UnliftIO              (MonadIO, STM, atomically, retrySTM)

data GrabItem tp = GrabItem
  { gFuncList :: IOMap FuncName ()
  , gAgent    :: CSEnv tp
  , gJobQueue :: IOMap JobHandle ()
  }

key :: GrabItem tp -> (Nid, Msgid)
key GrabItem{gAgent = a} = ident a

data GrabQueue tp = GrabQueue
  { items :: IOMap (Nid, Msgid) (GrabItem tp)
  , index :: IOMap FuncName [(Nid, Msgid)]
  }

newGrabQueue :: MonadIO m => m (GrabQueue tp)
newGrabQueue = do
  items <- IOMap.empty
  index <- IOMap.empty
  return GrabQueue {..}

pushAgent :: MonadIO m => GrabQueue tp -> IOMap FuncName () -> IOMap JobHandle () -> CSEnv tp -> m ()
pushAgent GrabQueue { .. } gFuncList gJobQueue gAgent = atomically $ do
  IOMapS.insert k v items
  funcs <- IOMapS.keys gFuncList
  mapM_ (\fn -> IOMapS.insertLookupWithKey f fn [k] index) funcs
  where v = GrabItem {..}
        k = key v
        f _ nv ov = nub $ nv ++ ov

popAgentSTM :: GrabQueue tp -> FuncName -> STM (IOMap JobHandle (), CSEnv tp)
popAgentSTM GrabQueue { .. } fn = do
  ks <- IOMapS.insertLookupWithKey f fn [] index
  case ks of
    Nothing    -> retrySTM
    Just []    -> retrySTM
    Just (x:_) -> do
      mid <- IOMapS.lookupIndex x items
      case mid of
        Nothing -> retrySTM
        Just idx -> do
          (_, item) <- IOMapS.elemAt idx items
          IOMapS.deleteAt idx items
          return (gJobQueue item, gAgent item)

  where f _ _ []     = []
        f _ _ (_:xs) = xs

popAgentList :: MonadIO m => GrabQueue tp -> FuncName -> m [(IOMap JobHandle (), CSEnv tp)]
popAgentList GrabQueue { .. } fn = atomically $ do
  ks <- IOMapS.insertLookupWithKey f fn [] index
  case ks of
    Nothing -> pure []
    Just xs -> catMaybes <$> mapM mapFunc xs

  where f _ _ _ = []
        mapFunc x = do
          mid <- IOMapS.lookupIndex x items
          case mid of
            Nothing -> pure Nothing
            Just idx -> do
              (_, item) <- IOMapS.elemAt idx items
              IOMapS.deleteAt idx items
              return $ Just (gJobQueue item, gAgent item)
