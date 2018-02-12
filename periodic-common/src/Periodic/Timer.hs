{-# LANGUAGE RecordWildCards #-}

module Periodic.Timer
  (
    Timer
  , newTimer
  , startTimer
  , startTimer'
  , repeatTimer
  , repeatTimer'
  , clearTimer
  ) where


import           Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import           Control.Monad      (forever, when)
import           Data.IORef         (IORef, atomicModifyIORef', newIORef)
import qualified Periodic.Lock      as L

data Timer = Timer { timer  :: IORef ThreadId
                   , locker :: L.Lock
                   , state  :: IORef Bool
                   }

newTimer :: IO Timer
newTimer = do
  timer  <- newIORef $ error "not started"
  locker <- L.new
  state <- newIORef False
  return Timer {..}

clearTimer :: Timer -> IO ()
clearTimer Timer{..} = L.with locker $ do
  started <- atomicModifyIORef' state (\t -> (False, t))
  when (started) $ do
    t <- atomicModifyIORef' timer (\t -> (t, t))
    killThread t

-- | startTimer for a given number of microseconds
--
startTimer :: Timer -> Int -> IO () -> IO ()
startTimer tm@Timer{..} delay io = L.with locker $ do
  clearTimer tm
  atomicModifyIORef' state (const $ (True, ()))

  t <- forkIO $ do
    when (delay > 0) $ threadDelay delay
    io

  atomicModifyIORef' timer (const $ (t, ()))

-- | startTimer' for a given number of seconds
--
startTimer' :: Timer -> Int -> IO () -> IO ()
startTimer' t delay = startTimer t (delay * 1000000)

-- | repeatTimer for a given number of microseconds
--
repeatTimer :: Timer -> Int -> IO () -> IO ()
repeatTimer tm@Timer{..} delay io = L.with locker $ do
  clearTimer tm
  atomicModifyIORef' state (const $ (True, ()))

  t <- forkIO $ forever $ do
    when (delay > 0) $ threadDelay delay
    io

  atomicModifyIORef' timer (const $ (t, ()))


-- | repeatTimer' for a given number of seconds
--
repeatTimer' :: Timer -> Int -> IO () -> IO ()
repeatTimer' t delay = repeatTimer t (delay * 1000000)
