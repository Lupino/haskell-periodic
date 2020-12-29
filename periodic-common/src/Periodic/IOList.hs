module Periodic.IOList
  ( IOList
  , newIOList
  , insert
  , insertSTM
  , append
  , elem
  , elemSTM
  , delete
  , deleteSTM
  , toList
  , toListSTM
  , clearSTM
  , fromList
  ) where

import qualified Data.List as L
import           Prelude   hiding (elem)
import           UnliftIO  (MonadIO, STM, TVar, atomically, modifyTVar',
                            newTVarIO, readTVar, readTVarIO, writeTVar)


newtype IOList a = IOList (TVar [a])

newIOList :: MonadIO m => m (IOList a)
newIOList = IOList <$> newTVarIO []

fromList :: MonadIO m => [a] -> m (IOList a)
fromList l = IOList <$> newTVarIO l

insertSTM :: IOList a -> a -> STM ()
insertSTM (IOList h) a = modifyTVar' h $ \v -> a:v

insert :: MonadIO m => IOList a -> a -> m ()
insert h = atomically . insertSTM h

append :: MonadIO m => IOList a -> a -> m ()
append (IOList h) a = atomically . modifyTVar' h $ \v -> v ++ [a]

elem :: (Eq a, MonadIO m) => IOList a -> a -> m Bool
elem (IOList h) a = L.elem a <$> readTVarIO h

elemSTM :: (Eq a) => IOList a -> a -> STM Bool
elemSTM (IOList h) a = L.elem a <$> readTVar h

delete :: (Eq a, MonadIO m) => IOList a -> a -> m ()
delete (IOList h) a = atomically . modifyTVar' h $ L.delete a

deleteSTM :: (Eq a) => IOList a -> a -> STM ()
deleteSTM (IOList h) a = modifyTVar' h $ L.delete a

toList :: MonadIO m => IOList a -> m [a]
toList (IOList h) = readTVarIO h

toListSTM :: IOList a -> STM [a]
toListSTM (IOList h) = readTVar h

clearSTM :: IOList a -> STM ()
clearSTM (IOList h) = writeTVar h []
