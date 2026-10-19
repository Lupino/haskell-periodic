{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A long-lived-client benchmark for the server scheduling path.  It starts
-- an in-process echo worker and keeps one client connection per submitter, so
-- process startup and connection setup are outside the measured interval.
module Main (main) where

import           Control.Monad       (forM, forM_, replicateM, unless, void, when)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as B
import           Data.List           (sort)
import           Data.Word           (Word64)
import           GHC.Clock           (getMonotonicTimeNSec)
import           Periodic.Client
import           Periodic.Job        (workDone_, workload)
import           Periodic.Types      (FuncName (..), JobName (..), Workload (..))
import           Periodic.Worker     hiding (close)
import           System.Environment  (getArgs)
import           System.Exit         (exitFailure)
import           Text.Read           (readMaybe)
import           UnliftIO
import           UnliftIO.Concurrent (threadDelay)

data Options = Options
  { host         :: String
  , jobs         :: Int
  , clients      :: Int
  , workers      :: Int
  , payloadBytes :: Int
  }

defaultOptions :: Options
defaultOptions = Options
  { host = "unix:///tmp/periodic.sock"
  , jobs = 10000
  , clients = 16
  , workers = 16
  , payloadBytes = 128
  }

parseOptions :: [String] -> Either String Options
parseOptions = go defaultOptions
  where
    go opt [] = Right opt
    go opt ("--host":v:xs) = go opt {host = v} xs
    go opt ("--jobs":v:xs) = setPositive "--jobs" v (\n -> opt {jobs = n}) xs
    go opt ("--clients":v:xs) = setPositive "--clients" v (\n -> opt {clients = n}) xs
    go opt ("--workers":v:xs) = setPositive "--workers" v (\n -> opt {workers = n}) xs
    go opt ("--payload-bytes":v:xs) = setNonNegative "--payload-bytes" v (\n -> opt {payloadBytes = n}) xs
    go _ _ = Left "usage: periodic-bench [--host URI] [--jobs N] [--clients N] [--workers N] [--payload-bytes N]"

    setPositive flag raw update xs =
      case readMaybe raw of
        Just n | n > 0 -> go (update n) xs
        _ -> Left $ flag ++ " must be a positive integer"

    setNonNegative flag raw update xs =
      case readMaybe raw of
        Just n | n >= 0 -> go (update n) xs
        _ -> Left $ flag ++ " must be a non-negative integer"

main :: IO ()
main = do
  args <- getArgs
  case parseOptions args of
    Left err -> putStrLn err >> exitFailure
    Right opts -> run opts

run :: Options -> IO ()
run Options{..} = do
  let func = FuncName "periodic-bench-echo"
      rawPayload = BS.replicate payloadBytes 120
      payload = Workload rawPayload
  worker <- async $ startWorkerM host $ do
    ok <- addFunc func $ do
      echoPayload <- workload
      void $ workDone_ echoPayload
    unless ok $ liftIO $ throwString "benchmark worker registration failed"
    work workers
  clientEnvs <- replicateM clients $ open host
  -- Do not begin timing until the worker has registered and completed one
  -- real assignment. Registration and the first GRABJOB are asynchronous, so
  -- retry a bounded number of times instead of using a timing-dependent sleep.
  case clientEnvs of
    [] -> throwString "benchmark requires at least one client"
    warmupClient:_ -> do
      warmup <- waitForWorker warmupClient func payload 1000
      unless (warmup == Just rawPayload) $
        throwString "benchmark warm-up did not return the echo payload"
  latencyRef <- newMVar []
  failureRef <- newMVar (0 :: Int)
  start <- getMonotonicTimeNSec
  submitters <- forM [0 .. clients - 1] $ \clientId -> async $ do
    let client = clientEnvs !! clientId
    forM_ [clientId + 1, clientId + 1 + clients .. jobs] $ \jobId -> do
      t0 <- getMonotonicTimeNSec
      result <- tryAny $ runClientM client $ runJob func (JobName $ B.pack $ "bench-" ++ show jobId) payload 30
      t1 <- getMonotonicTimeNSec
      modifyMVar_ latencyRef $ pure . (elapsedMs t0 t1 :)
      let succeeded = case result of
            Right (Just response) -> response == rawPayload
            _                     -> False
      when (not succeeded) $
        modifyMVar_ failureRef $ pure . (+ 1)
  mapM_ wait submitters
  finish <- getMonotonicTimeNSec
  mapM_ (\client -> runClientM client close) clientEnvs
  cancel worker
  latencies <- sort <$> readMVar latencyRef
  failures <- readMVar failureRef
  let elapsed = fromIntegral (finish - start) / 1000000000 :: Double
      succeeded = jobs - failures
      throughput = if elapsed == 0 then 0 else fromIntegral succeeded / elapsed
  putStrLn $ "jobs=" ++ show jobs
  putStrLn $ "clients=" ++ show clients
  putStrLn $ "workers=" ++ show workers
  putStrLn $ "payload_bytes=" ++ show payloadBytes
  putStrLn $ "succeeded=" ++ show succeeded
  putStrLn $ "failed=" ++ show failures
  putStrLn $ "elapsed_seconds=" ++ show elapsed
  putStrLn $ "throughput_rps=" ++ show throughput
  putStrLn $ "latency_ms_p50=" ++ show (percentile 50 latencies)
  putStrLn $ "latency_ms_p95=" ++ show (percentile 95 latencies)
  putStrLn $ "latency_ms_p99=" ++ show (percentile 99 latencies)
  unless (failures == 0) exitFailure

waitForWorker :: ClientEnv -> FuncName -> Workload -> Int -> IO (Maybe BS.ByteString)
waitForWorker _ _ _ 0 = pure Nothing
waitForWorker client func payload attempts = do
  result <- runClientM client $ runJob func (JobName "bench-warmup") payload 30
  case result of
    Just response -> pure $ Just response
    Nothing -> do
      threadDelay 10000
      waitForWorker client func payload (attempts - 1)

elapsedMs :: Word64 -> Word64 -> Double
elapsedMs start finish = fromIntegral (finish - start) / 1000000

percentile :: Int -> [Double] -> Double
percentile _ [] = 0
percentile n values = values !! min (length values - 1) ((length values * n + 99) `div` 100 - 1)
