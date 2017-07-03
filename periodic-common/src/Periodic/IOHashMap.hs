{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

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
  ) where

import           Prelude               hiding (lookup, null)

import           Data.ByteString.Char8 (ByteString)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM
import           Data.IORef            (IORef, atomicModifyIORef', newIORef)

newtype IOHashMap a = IOHashMap (IORef (HashMap ByteString a))

newIOHashMap :: IO (IOHashMap a)
newIOHashMap = IOHashMap <$> newIORef HM.empty

insert :: IOHashMap a -> ByteString -> a -> IO ()
insert (IOHashMap h) k v = atomicModifyIORef' h $ \m -> (HM.insert k v m, ())

delete :: IOHashMap a -> ByteString -> IO ()
delete (IOHashMap h) k = atomicModifyIORef' h $ \m -> (HM.delete k m, ())

lookup :: IOHashMap a -> ByteString -> IO (Maybe a)
lookup (IOHashMap h) k = atomicModifyIORef' h $ \m -> (m, HM.lookup k m)

adjust :: IOHashMap a -> (a -> a) -> ByteString -> IO ()
adjust (IOHashMap h) f k = atomicModifyIORef' h $ \m -> (HM.adjust f k m, ())

update :: IOHashMap a -> (a -> Maybe a) -> ByteString -> IO ()
update (IOHashMap h) f k = atomicModifyIORef' h $ \m -> (HM.update f k m, ())

alter :: IOHashMap a -> (Maybe a -> Maybe a) -> ByteString -> IO ()
alter (IOHashMap h) f k = atomicModifyIORef' h $ \m -> (HM.alter f k m, ())

null :: IOHashMap a -> IO Bool
null (IOHashMap h) = atomicModifyIORef' h $ \m -> (m, HM.null m)

size :: IOHashMap a -> IO Int
size (IOHashMap h) = atomicModifyIORef' h $ \m -> (m, HM.size m)

member :: IOHashMap a -> ByteString -> IO Bool
member (IOHashMap h) k = atomicModifyIORef' h $ \m -> (m, HM.member k m)

keys :: IOHashMap a -> IO [ByteString]
keys (IOHashMap h) = atomicModifyIORef' h $ \m -> (m, HM.keys m)

elems :: IOHashMap a -> IO [a]
elems (IOHashMap h) = atomicModifyIORef' h $ \m -> (m, HM.elems m)
