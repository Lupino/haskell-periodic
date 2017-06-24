{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.FuncList
  (
    FuncName
  , FuncList
  , newFuncList
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

type FuncName = ByteString

newtype FuncList a = FuncList (IORef (HashMap FuncName a))

newFuncList :: IO (FuncList a)
newFuncList = FuncList <$> newIORef HM.empty

insert :: FuncList a -> FuncName -> a -> IO ()
insert (FuncList h) k v = atomicModifyIORef' h $ \m -> (HM.insert k v m, ())

delete :: FuncList a -> FuncName -> IO ()
delete (FuncList h) k = atomicModifyIORef' h $ \m -> (HM.delete k m, ())

lookup :: FuncList a -> FuncName -> IO (Maybe a)
lookup (FuncList h) k = atomicModifyIORef' h $ \m -> (m, HM.lookup k m)

adjust :: FuncList a -> (a -> a) -> FuncName -> IO ()
adjust (FuncList h) f k = atomicModifyIORef' h $ \m -> (HM.adjust f k m, ())

update :: FuncList a -> (a -> Maybe a) -> FuncName -> IO ()
update (FuncList h) f k = atomicModifyIORef' h $ \m -> (HM.update f k m, ())

alter :: FuncList a -> (Maybe a -> Maybe a) -> FuncName -> IO ()
alter (FuncList h) f k = atomicModifyIORef' h $ \m -> (HM.alter f k m, ())

null :: FuncList a -> IO Bool
null (FuncList h) = atomicModifyIORef' h $ \m -> (m, HM.null m)

size :: FuncList a -> IO Int
size (FuncList h) = atomicModifyIORef' h $ \m -> (m, HM.size m)

member :: FuncList a -> FuncName -> IO Bool
member (FuncList h) k = atomicModifyIORef' h $ \m -> (m, HM.member k m)

keys :: FuncList a -> IO [FuncName]
keys (FuncList h) = atomicModifyIORef' h $ \m -> (m, HM.keys m)

elems :: FuncList a -> IO [a]
elems (FuncList h) = atomicModifyIORef' h $ \m -> (m, HM.elems m)
