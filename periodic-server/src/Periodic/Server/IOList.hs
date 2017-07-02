module Periodic.Server.IOList
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

import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.List  as L
import           Prelude    hiding (elem)


newtype IOList a = IOList (IORef [a])

modify :: IOList a -> ([a] -> ([a], b)) -> IO b
modify (IOList h) f = atomicModifyIORef' h f

newIOList :: IO (IOList a)
newIOList = IOList <$> newIORef []

fromList :: [a] -> IO (IOList a)
fromList l = IOList <$> newIORef l

insert :: IOList a -> a -> IO ()
insert h a = modify h $ \v -> (a:v, ())

append :: IOList a -> a -> IO ()
append h a = modify h $ \v -> (concat [v, [a]], ())

elem :: (Eq a) => IOList a -> a -> IO Bool
elem h a = modify h $ \v -> (v, L.elem a v)

delete :: (Eq a) => IOList a -> a -> IO ()
delete h a = modify h $ \v -> (L.delete a v, ())

toList :: IOList a -> IO [a]
toList h = modify h $ \v -> (v, v)
