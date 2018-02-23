module Periodic.IOHashMap
  (
    IOHashMap
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

  , lookupSTM
  ) where

import           Prelude                     hiding (lookup, null)

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM           (STM, atomically)
import           Data.Hashable
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM

newtype IOHashMap a b = IOHashMap (TVar (HashMap a b))

newIOHashMap :: IO (IOHashMap a b)
newIOHashMap = IOHashMap <$> newTVarIO HM.empty

insert :: (Eq a, Hashable a) => IOHashMap a b -> a -> b -> IO ()
insert (IOHashMap h) k v = atomically . modifyTVar' h $ HM.insert k v

delete :: (Eq a, Hashable a) => IOHashMap a b -> a -> IO ()
delete (IOHashMap h) k = atomically . modifyTVar' h $ HM.delete k

lookup :: (Eq a, Hashable a) => IOHashMap a b -> a -> IO (Maybe b)
lookup (IOHashMap h) k = HM.lookup k <$> readTVarIO h

adjust :: (Eq a, Hashable a) => IOHashMap a b -> (b -> b) -> a -> IO ()
adjust (IOHashMap h) f k = atomically . modifyTVar' h $ HM.adjust f k

update :: (Eq a, Hashable a) => IOHashMap a b -> (b -> Maybe b) -> a -> IO ()
update (IOHashMap h) f k = atomically . modifyTVar' h $ HM.update f k

alter :: (Eq a, Hashable a) => IOHashMap a b -> (Maybe b -> Maybe b) -> a -> IO ()
alter (IOHashMap h) f k = atomically . modifyTVar' h $ HM.alter f k

null :: IOHashMap a b -> IO Bool
null (IOHashMap h) = HM.null <$> readTVarIO h

size :: IOHashMap a b -> IO Int
size (IOHashMap h) = HM.size <$> readTVarIO h

member :: (Eq a, Hashable a) => IOHashMap a b -> a -> IO Bool
member (IOHashMap h) k = HM.member k <$> readTVarIO h

keys :: IOHashMap a b -> IO [a]
keys (IOHashMap h) = HM.keys <$> readTVarIO h

elems :: IOHashMap a b -> IO [b]
elems (IOHashMap h) = HM.elems <$> readTVarIO h

clear :: IOHashMap a b -> IO ()
clear (IOHashMap h) = atomically . modifyTVar' h $ const HM.empty

toList :: IOHashMap a b -> IO [(a, b)]
toList (IOHashMap h) = HM.toList <$> readTVarIO h

lookupSTM :: (Eq a, Hashable a) => IOHashMap a b -> a -> STM (Maybe b)
lookupSTM (IOHashMap h) k = HM.lookup k <$> readTVar h
