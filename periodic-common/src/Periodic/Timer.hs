{-# LANGUAGE RecordWildCards #-}

module Periodic.Timer
  (
    Timer
  , newTimer
  , initTimer
  , startTimer
  , startTimer'
  , repeatTimer
  , repeatTimer'
  , clearTimer
  ) where


import           Control.Concurrent      (ThreadId, forkIO, killThread,
                                          threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (forever, when)
import           Data.IORef              (IORef, atomicModifyIORef', newIORef)
import           Data.Maybe              (fromJust, isJust, isNothing)
import qualified Periodic.Lock           as L

data Timer = Timer { timer  :: IORef (Maybe ThreadId)
                   , waiter :: MVar ()
                   , locker :: L.Lock
                   , runner :: IORef (Maybe ThreadId)
                   }

newTimer :: IO Timer
newTimer = do
  timer  <- newIORef Nothing
  waiter <- newEmptyMVar
  locker <- L.new
  runner <- newIORef Nothing

  return Timer {..}

initTimer :: Timer -> IO () -> IO ()
initTimer (Timer {..}) io = L.with locker $ do
  t <- atomicModifyIORef' runner (\v -> (v, v))
  when (isNothing t) $ do
    t' <- forkIO $ forever $ do
      takeMVar waiter
      io

    atomicModifyIORef' runner (\_ -> (Just t', ()))

clearTimer :: Timer -> IO ()
clearTimer Timer {..} = do
  t <- atomicModifyIORef' timer (\v -> (v, v))
  when (isJust t) $ killThread (fromJust t)
  t' <- atomicModifyIORef' runner (\v -> (v, v))
  when (isJust t') $ killThread (fromJust t')

-- | startTimer for a given number of microseconds
--
startTimer :: Timer -> Int -> IO ()
startTimer (Timer {..}) delay = L.with locker $ do
  t <- atomicModifyIORef' timer (\v -> (v, v))
  when (isJust t) $ killThread (fromJust t)

  t' <- forkIO $ do
    when (delay > 0) $ threadDelay $ delay
    putMVar waiter ()

  atomicModifyIORef' timer (\_ -> (Just t', ()))

-- | startTimer' for a given number of seconds
--
startTimer' :: Timer -> Int -> IO ()
startTimer' t delay = startTimer t (delay * 1000000)

-- | repeatTimer for a given number of microseconds
--
repeatTimer :: Timer -> Int -> IO ()
repeatTimer (Timer {..}) delay = L.with locker $ do
  t <- atomicModifyIORef' timer (\v -> (v, v))
  when (isJust t) $ killThread (fromJust t)

  t' <- forkIO $ forever $ do
    when (delay > 0) $ threadDelay $ delay
    putMVar waiter ()

  atomicModifyIORef' timer (\_ -> (Just t', ()))

-- | repeatTimer' for a given number of seconds
--
repeatTimer' :: Timer -> Int -> IO ()
repeatTimer' t delay = startTimer t (delay * 1000000)
