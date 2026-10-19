{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Periodic.Server.SchedPool
import           Periodic.Types.Job
import           UnliftIO (newTVarIO)

assert :: String -> Bool -> IO ()
assert message condition =
  if condition then pure () else error message

main :: IO ()
main = do
  maxWait <- newTVarIO 10
  pool <- newSchedPool maxWait (pure ())
  let job = initJob "sched-pool-test" "indexed-handle"
      handle = getHandle job
  assert "a new pool must not report an unknown handle" =<< fmap not (hasHandle pool handle)
  spawn pool job
  assert "spawned active jobs must be found through the handle index" =<< hasHandle pool handle
