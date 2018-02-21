module Periodic.IOList
  (
    IOList
  , newIOList
  , insert
  , append
  , elem
  , delete
  , toList
  , fromList
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad.STM           (atomically)
import qualified Data.List                   as L
import           Prelude                     hiding (elem)


newtype IOList a = IOList (TVar [a])

newIOList :: IO (IOList a)
newIOList = IOList <$> newTVarIO []

fromList :: [a] -> IO (IOList a)
fromList l = IOList <$> newTVarIO l

insert :: IOList a -> a -> IO ()
insert (IOList h) a = atomically . modifyTVar' h $ \v -> a:v

append :: IOList a -> a -> IO ()
append (IOList h) a = atomically . modifyTVar' h $ \v -> v ++ [a]

elem :: (Eq a) => IOList a -> a -> IO Bool
elem (IOList h) a = L.elem a <$> readTVarIO h

delete :: (Eq a) => IOList a -> a -> IO ()
delete (IOList h) a = atomically . modifyTVar' h $ L.delete a

toList :: IOList a -> IO [a]
toList (IOList h) = readTVarIO h
