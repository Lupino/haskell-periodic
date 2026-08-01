{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Periodic.Server.Persist.Workload
import           Periodic.Types.Job
import           Control.Exception       (IOException, try)
import qualified Data.ByteString         as BS
import           Data.List               ((\\), sort)
import           System.Directory        (doesDirectoryExist, listDirectory)
import           System.FilePath         ((</>))

assert :: String -> Bool -> IO ()
assert message condition =
  if condition then pure () else error message

main :: IO ()
main = do
  store <- newWorkloadStore "test"
  rootExists <- doesDirectoryExist "periodic-workloads/test"
  assert "workload directory should remain available for later jobs" rootExists

  let empty = initJob "workload-test" "empty"
  emptyStored <- storeWorkload store empty
  assert "empty workloads remain inline" (getWorkload emptyStored == getWorkload empty)

  let small = setWorkload (Workload $ BS.replicate 1024 97) $ initJob "workload-test" "inline"
  beforeSmall <- sort <$> listDirectory "periodic-workloads/test"
  smallStored <- storeWorkload store small
  afterSmall <- sort <$> listDirectory "periodic-workloads/test"
  smallRestored <- loadWorkload store smallStored
  assert "1 KiB workloads should not create a file" (afterSmall == beforeSmall)
  assert "inline workloads should restore their original body" (getWorkload smallRestored == getWorkload small)

  let large = setWorkload (Workload $ BS.replicate 1025 98) $ initJob "workload-test" "file-reference"
  removeWorkload store =<< storeWorkload store large
  beforeLarge <- sort <$> listDirectory "periodic-workloads/test"
  largeStored <- storeWorkload store large
  afterLarge <- sort <$> listDirectory "periodic-workloads/test"
  largeRestored <- loadWorkload store largeStored
  assert "large workloads should be replaced with a file marker" (getWorkload largeStored /= getWorkload large)
  assert "large workloads should restore their original body" (getWorkload largeRestored == getWorkload large)
  let createdFiles = afterLarge \\ beforeLarge
  assert "large workloads should create exactly one file" (length createdFiles == 1)

  smallReplacement <- storeWorkload store $ setWorkload "small" large
  afterReplacement <- sort <$> listDirectory "periodic-workloads/test"
  replacementRestored <- loadWorkload store smallReplacement
  assert "small replacements should remove the old workload file" (afterReplacement == beforeLarge)
  assert "small replacements should restore their original body" (getWorkload replacementRestored == getWorkload (setWorkload "small" large))

  largeForChecksum <- storeWorkload store large
  checksumFiles <- sort <$> listDirectory "periodic-workloads/test"
  let checksumCreated = checksumFiles \\ beforeLarge
  assert "checksum test should create one workload file" (length checksumCreated == 1)
  BS.writeFile ("periodic-workloads/test" </> head checksumCreated) "corrupt"
  checksumResult <- try (loadWorkload store largeForChecksum) :: IO (Either IOException Job)
  assert "checksum mismatch should fail" (case checksumResult of Left _ -> True; Right _ -> False)
  removeWorkload store largeForChecksum
