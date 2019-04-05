module Periodic.IOHashMap
  ( IOHashMap
  , newIOHashMap
  , insert
  , delete
  , lookup
  , update
  , adjust
  , alter
  , null
  , size
  , member
  , keys
  , elems
  , clear
  , toList

  , insertSTM
  , lookupSTM
  , foldrWithKeySTM
  , deleteSTM
  ) where

import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Prelude             hiding (lookup, null)
import           UnliftIO            (MonadIO (..), STM, TVar, atomically,
                                      modifyTVar', newTVarIO, readTVar,
                                      readTVarIO)

newtype IOHashMap a b = IOHashMap (TVar (HashMap a b))

newIOHashMap :: MonadIO m => m (IOHashMap a b)
newIOHashMap = IOHashMap <$> newTVarIO HM.empty

insert :: (Eq a, Hashable a, MonadIO m) => IOHashMap a b -> a -> b -> m ()
insert (IOHashMap h) k v = atomically . modifyTVar' h $ HM.insert k v

delete :: (Eq a, Hashable a, MonadIO m) => IOHashMap a b -> a -> m ()
delete (IOHashMap h) k = atomically . modifyTVar' h $ HM.delete k

lookup :: (Eq a, Hashable a, MonadIO m) => IOHashMap a b -> a -> m (Maybe b)
lookup (IOHashMap h) k = HM.lookup k <$> readTVarIO h

adjust :: (Eq a, Hashable a, MonadIO m) => IOHashMap a b -> (b -> b) -> a -> m ()
adjust (IOHashMap h) f k = atomically . modifyTVar' h $ HM.adjust f k

update :: (Eq a, Hashable a, MonadIO m) => IOHashMap a b -> (b -> Maybe b) -> a -> m ()
update (IOHashMap h) f k = atomically . modifyTVar' h $ HM.update f k

alter :: (Eq a, Hashable a, MonadIO m) => IOHashMap a b -> (Maybe b -> Maybe b) -> a -> m ()
alter (IOHashMap h) f k = atomically . modifyTVar' h $ HM.alter f k

null :: MonadIO m => IOHashMap a b -> m Bool
null (IOHashMap h) = HM.null <$> readTVarIO h

size :: MonadIO m => IOHashMap a b -> m Int
size (IOHashMap h) = HM.size <$> readTVarIO h

member :: (Eq a, Hashable a, MonadIO m) => IOHashMap a b -> a -> m Bool
member (IOHashMap h) k = HM.member k <$> readTVarIO h

keys :: MonadIO m => IOHashMap a b -> m [a]
keys (IOHashMap h) = HM.keys <$> readTVarIO h

elems :: MonadIO m => IOHashMap a b -> m [b]
elems (IOHashMap h) = HM.elems <$> readTVarIO h

clear :: MonadIO m => IOHashMap a b -> m ()
clear (IOHashMap h) = atomically . modifyTVar' h $ const HM.empty

toList :: MonadIO m => IOHashMap a b -> m [(a, b)]
toList (IOHashMap h) = HM.toList <$> readTVarIO h

insertSTM :: (Eq a, Hashable a) => IOHashMap a b -> a -> b -> STM ()
insertSTM (IOHashMap h) k v = modifyTVar' h $ HM.insert k v

lookupSTM :: (Eq a, Hashable a) => IOHashMap a b -> a -> STM (Maybe b)
lookupSTM (IOHashMap h) k = HM.lookup k <$> readTVar h

foldrWithKeySTM :: IOHashMap a b -> (a -> b -> c -> c) -> c -> STM c
foldrWithKeySTM (IOHashMap h) f acc = HM.foldrWithKey f acc <$> readTVar h

deleteSTM :: (Eq a, Hashable a) => IOHashMap a b -> a -> STM ()
deleteSTM (IOHashMap h) k = modifyTVar' h $ HM.delete k
