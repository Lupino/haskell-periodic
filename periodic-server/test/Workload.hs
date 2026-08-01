{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Periodic.Server.Persist.Workload
import           Periodic.Types.Job
import           System.Directory        (doesDirectoryExist)

assert :: String -> Bool -> IO ()
assert message condition =
  if condition then pure () else error message

main :: IO ()
main = do
  store <- newWorkloadStore "test"
  let original = setWorkload "workload body" $ initJob "workload-test" "file-reference"
  stored <- storeWorkload store original
  restored <- loadWorkload store stored
  assert "stored Job should no longer contain the workload body" (getWorkload stored /= getWorkload original)
  assert "loaded Job should restore the workload body" (getWorkload restored == getWorkload original)
  removeWorkload store stored
  rootExists <- doesDirectoryExist "periodic-workloads/test"
  assert "workload directory should remain available for later jobs" rootExists
  let empty = initJob "workload-test" "empty"
  emptyStored <- storeWorkload store empty
  assert "empty workloads remain inline" (getWorkload emptyStored == getWorkload empty)
