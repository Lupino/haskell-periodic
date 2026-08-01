{-# LANGUAGE OverloadedStrings #-}

module Periodic.Server.Persist.Workload
  ( WorkloadStore
  , newWorkloadStore
  , storeWorkload
  , loadWorkload
  , removeWorkload
  ) where

import           Control.Exception      (bracketOnError)
import           Crypto.Hash             (Digest, SHA256 (SHA256), hashWith)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as B8
import           Data.Char               (isDigit)
import           Data.Maybe              (listToMaybe)
import           Periodic.Types.Job      (FuncName (..), Job, JobName (..),
                                          Workload (..), getFuncName, getName,
                                          getWorkload, setWorkload)
import           System.Directory        (createDirectoryIfMissing, doesFileExist,
                                          pathIsSymbolicLink, removeFile,
                                          renameFile)
import           System.FilePath         ((</>), takeDirectory)
import           System.IO               (hClose, hFlush, hSetBinaryMode,
                                          openBinaryTempFile)

newtype WorkloadStore = WorkloadStore FilePath

-- Markers live inside the existing Workload field, keeping Job's binary format
-- and the jobs table unchanged.  Inline data is marked as well so arbitrary
-- workload bytes can never be mistaken for a file reference.
fileMarkerPrefix :: ByteString
fileMarkerPrefix = "\NULperiodic-workload-file-v1\NUL"

inlineMarkerPrefix :: ByteString
inlineMarkerPrefix = "\NULperiodic-workload-inline-v1\NUL"

inlineWorkloadLimit :: Int
inlineWorkloadLimit = 1024

newWorkloadStore :: FilePath -> IO WorkloadStore
newWorkloadStore namespace = do
  let root = "periodic-workloads" </> namespace
  createDirectoryIfMissing True root
  pure $ WorkloadStore root

storeWorkload :: WorkloadStore -> Job -> IO Job
storeWorkload store job
  | BS.null body = pure job
  | BS.length body <= inlineWorkloadLimit = do
      removeStaleWorkload store job
      pure $ setWorkload (Workload $ inlineMarkerPrefix <> body) job
  | otherwise = do
      let path = workloadPath store job
          marker = markerFor job body
      writeAtomically path body
      pure $ setWorkload (Workload marker) job
  where Workload body = getWorkload job

loadWorkload :: WorkloadStore -> Job -> IO Job
loadWorkload store job =
  case BS.stripPrefix inlineMarkerPrefix raw of
    Just body -> pure $ setWorkload (Workload body) job
    Nothing ->
      case parseFileMarker raw of
        Nothing -> pure job
        Just (fileName, digest, size)
          | fileName /= workloadFileName job -> ioError $ userError "Invalid workload file reference"
          | otherwise -> do
              let path = workloadPath store job
              exists <- doesFileExist path
              linked <- if exists then pathIsSymbolicLink path else pure False
              if not exists || linked
                then ioError $ userError "Workload file is missing or unsafe"
                else do
                  body <- BS.readFile path
                  if BS.length body /= size || sha256 body /= digest
                    then ioError $ userError "Workload file checksum mismatch"
                    else pure $ setWorkload (Workload body) job
  where raw = unWL (getWorkload job)

removeWorkload :: WorkloadStore -> Job -> IO ()
removeWorkload store job =
  case parseFileMarker $ unWL (getWorkload job) of
    Just (fileName, _, _)
      | fileName == workloadFileName job -> removeStaleWorkload store job
    _ -> pure ()

workloadPath :: WorkloadStore -> Job -> FilePath
workloadPath (WorkloadStore root) job = root </> B8.unpack (workloadFileName job)

workloadFileName :: Job -> ByteString
workloadFileName job = sha256 (unFN (getFuncName job) <> "\NUL" <> unJN (getName job)) <> ".workload"

markerFor :: Job -> ByteString -> ByteString
markerFor job body =
  B8.intercalate ":"
    [ fileMarkerPrefix <> workloadFileName job
    , sha256 body
    , B8.pack $ show (BS.length body)
    ]

parseFileMarker :: ByteString -> Maybe (ByteString, ByteString, Int)
parseFileMarker raw = do
  suffix <- BS.stripPrefix fileMarkerPrefix raw
  case B8.split ':' suffix of
    [fileName, digest, rawSize]
      | validFileName fileName
      , BS.length digest == 64
      , B8.all isHexDigit digest
      , B8.all isDigit rawSize -> do
          size <- readMaybeInt rawSize
          pure (fileName, digest, size)
    _ -> Nothing
  where
    isHexDigit c = isDigit c || c `elem` ("abcdef" :: String)

    validFileName fileName =
      BS.length fileName == 73
        && B8.all isHexDigit (BS.take 64 fileName)
        && BS.drop 64 fileName == ".workload"

readMaybeInt :: ByteString -> Maybe Int
readMaybeInt = fmap fst . listToMaybe . reads . B8.unpack

sha256 :: ByteString -> ByteString
sha256 = B8.pack . show . (hashWith SHA256 :: ByteString -> Digest SHA256)

removeStaleWorkload :: WorkloadStore -> Job -> IO ()
removeStaleWorkload store job = do
  let path = workloadPath store job
  exists <- doesFileExist path
  linked <- if exists then pathIsSymbolicLink path else pure False
  if not exists || linked then pure () else removeFile path

writeAtomically :: FilePath -> ByteString -> IO ()
writeAtomically path body = do
  let directory = takeDirectory path
  bracketOnError
    (openBinaryTempFile directory ".workload.tmp")
    (\(tmp, handle) -> hClose handle >> removeFile tmp)
    (\(tmp, handle) -> do
      hSetBinaryMode handle True
      BS.hPut handle body
      hFlush handle
      hClose handle
      renameFile tmp path)
