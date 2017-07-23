{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Periodic.Monad
  (
    Result (..)
  , GenPeriodic (..)
  , runPeriodic
  , env
  , unsafeLiftIO
  , unsafeLiftIO'
  ) where

import           Control.Exception      (Exception (..), SomeException, throw)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Typeable          (Typeable)

newtype MonadFail = MonadFail String
  deriving (Typeable, Show)

instance Exception MonadFail

newtype GenPeriodic u a = GenPeriodic { unPeriodic :: u -> IO (Result a) }

data Result a = Done a
              | Throw SomeException

instance Monad (GenPeriodic u) where
  return a = GenPeriodic $ \_ -> return (Done a)
  GenPeriodic m >>= k = GenPeriodic $ \env -> do
    e <- m env
    case e of
      Done a  -> unPeriodic (k a) env
      Throw e -> return (Throw e)

  fail msg = GenPeriodic $ \_ -> return $ Throw $ toException $ MonadFail $ msg

  (>>) = (*>)

instance Functor (GenPeriodic u) where
  fmap f (GenPeriodic m) = GenPeriodic $ \env -> do
    e <- m env
    case e of
      Done a  -> return (Done (f a))
      Throw e -> return (Throw e)

instance Applicative (GenPeriodic u) where
  pure = return
  GenPeriodic f <*> GenPeriodic a = GenPeriodic $ \env -> do
    r <- f env
    case r of
      Throw e -> return (Throw e)
      Done f' -> do
        ra <- a env
        case ra of
          Done a' -> return (Done (f' a'))
          Throw e -> return (Throw e)

runPeriodic :: u -> GenPeriodic u a -> IO a
runPeriodic env (GenPeriodic m) = do
  e <- m env
  case e of
    Done a  -> return a
    Throw e -> throw e

env :: GenPeriodic u u
env = GenPeriodic $ \env -> return (Done env)

-- Unsafe operations

-- | Under ordinary circumstances this is unnecessary; users of the Periodic
-- monad should generally /not/ perform arbitrary IO.
unsafeLiftIO :: IO a -> GenPeriodic u a
unsafeLiftIO m = GenPeriodic $ \_ -> Done <$> m

instance MonadIO (GenPeriodic u) where
  liftIO = unsafeLiftIO

unsafeLiftIO' :: (u -> IO a) -> GenPeriodic u a
unsafeLiftIO' f = GenPeriodic $ \u -> Done <$> f u
