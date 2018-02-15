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
  ) where

import           Prelude             hiding (lookup, null)

import           Data.ByteString     (ByteString)
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.IORef          (IORef, atomicModifyIORef', newIORef)

newtype IOHashMap a b = IOHashMap (IORef (HashMap a b))

newIOHashMap :: IO (IOHashMap a b)
newIOHashMap = IOHashMap <$> newIORef HM.empty

insert :: (Eq a, Hashable a) => IOHashMap a b -> a -> b -> IO ()
insert (IOHashMap h) k v = atomicModifyIORef' h $ \m -> (HM.insert k v m, ())

delete :: (Eq a, Hashable a) => IOHashMap a b -> a -> IO ()
delete (IOHashMap h) k = atomicModifyIORef' h $ \m -> (HM.delete k m, ())

lookup :: (Eq a, Hashable a) => IOHashMap a b -> a -> IO (Maybe b)
lookup (IOHashMap h) k = atomicModifyIORef' h $ \m -> (m, HM.lookup k m)

adjust :: (Eq a, Hashable a) => IOHashMap a b -> (b -> b) -> a -> IO ()
adjust (IOHashMap h) f k = atomicModifyIORef' h $ \m -> (HM.adjust f k m, ())

update :: (Eq a, Hashable a) => IOHashMap a b -> (b -> Maybe b) -> a -> IO ()
update (IOHashMap h) f k = atomicModifyIORef' h $ \m -> (HM.update f k m, ())

alter :: (Eq a, Hashable a) => IOHashMap a b -> (Maybe b -> Maybe b) -> a -> IO ()
alter (IOHashMap h) f k = atomicModifyIORef' h $ \m -> (HM.alter f k m, ())

null :: IOHashMap a b -> IO Bool
null (IOHashMap h) = atomicModifyIORef' h $ \m -> (m, HM.null m)

size :: IOHashMap a b -> IO Int
size (IOHashMap h) = atomicModifyIORef' h $ \m -> (m, HM.size m)

member :: (Eq a, Hashable a) => IOHashMap a b -> a -> IO Bool
member (IOHashMap h) k = atomicModifyIORef' h $ \m -> (m, HM.member k m)

keys :: IOHashMap a b -> IO [a]
keys (IOHashMap h) = atomicModifyIORef' h $ \m -> (m, HM.keys m)

elems :: IOHashMap a b -> IO [b]
elems (IOHashMap h) = atomicModifyIORef' h $ \m -> (m, HM.elems m)

clear :: IOHashMap a b -> IO ()
clear (IOHashMap h) = atomicModifyIORef' h $ const (HM.empty, ())
