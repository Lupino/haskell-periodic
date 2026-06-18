{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM_)
import Data.Int (Int64)
import qualified Data.ByteString.Char8 as B
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (doesFileExist, removeFile)
import Text.Printf (printf)

import Periodic.Server.Persist
import Periodic.Server.Persist.Memory (Memory, memorySize, useMemory)
import Periodic.Server.Persist.SQLite (SQLite, useSQLite)
import Periodic.Types.Job

main :: IO ()
main = do
  putStrLn "== Persist Read/Write Benchmark =="
  putStrLn ""
  benchPollPath
  putStrLn ""
  benchMetricPath
  putStrLn ""
  benchMemorySize

removeIfExists :: FilePath -> IO ()
removeIfExists path = do
  exists <- doesFileExist path
  if exists then removeFile path else pure ()

resetSQLiteFile :: FilePath -> IO ()
resetSQLiteFile dbFile = do
  removeIfExists dbFile
  removeIfExists $ dbFile ++ "-wal"
  removeIfExists $ dbFile ++ "-shm"

benchPollPath :: IO ()
benchPollPath = do
  let dbFile = "/private/tmp/periodic-persist-bench.sqlite"
      funcName = FuncName "bench-poll"
      ts = 9999999999 :: Int64
      totalJobs = 20000 :: Int
      batchSize = 400 :: Int
      rounds = 3000 :: Int

  resetSQLiteFile dbFile

  db <- newPersist (useSQLite dbFile) :: IO SQLite

  putStrLn "[1/2] Seeding SQLite jobs for poll benchmark..."
  forM_ [1 .. totalJobs] $ \i -> do
    let jn = JobName (B.pack ("job-" ++ show i))
        job = setSchedAt 1 (initJob funcName jn)
    insert db Pending job

  putStrLn "[1/2] Running poll path benchmark..."

  tLegacy <- timeIt $ replicateM_ rounds $ do
    c <- countPending db funcName ts
    if c > 0
      then do
        xs <- getPendingJob db funcName ts batchSize
        _ <- evaluate (length xs)
        pure ()
      else pure ()

  tNew <- timeIt $ replicateM_ rounds $ do
    xs <- getPendingJob db funcName ts batchSize
    _ <- evaluate (length xs)
    pure ()

  printf "poll legacy (count+get): %.2f ms (%.2f us/op)\n"
    (toMs tLegacy) (toUs tLegacy / fromIntegral rounds)
  printf "poll current (get only): %.2f ms (%.2f us/op)\n"
    (toMs tNew) (toUs tNew / fromIntegral rounds)
  printf "poll speedup: %.2fx\n" (tLegacy / tNew)

benchMetricPath :: IO ()
benchMetricPath = do
  let dbFileSingle = "/private/tmp/periodic-metric-single-bench.sqlite"
      dbFileBatch = "/private/tmp/periodic-metric-batch-bench.sqlite"
      totalMetrics = 20000 :: Int
      batchSize = 400 :: Int
      metrics =
        [ ("benchEvent", "benchMetric", i `mod` 1000)
        | i <- [1 .. totalMetrics]
        ]

  resetSQLiteFile dbFileSingle
  resetSQLiteFile dbFileBatch

  dbSingle <- newPersist (useSQLite dbFileSingle) :: IO SQLite
  dbBatch <- newPersist (useSQLite dbFileBatch) :: IO SQLite

  putStrLn "[metric] Running SQLite metric insert benchmark..."

  tSingle <- timeIt $
    forM_ metrics $ \(event, name, durationMs) ->
      insertMetric dbSingle event name durationMs

  tBatch <- timeIt $
    forM_ (chunksOf batchSize metrics) $
      insertMetrics dbBatch

  printf "metric single inserts: %.2f ms (%.2f us/op)\n"
    (toMs tSingle) (toUs tSingle / fromIntegral totalMetrics)
  printf "metric batch inserts: %.2f ms (%.2f us/op, batch=%d)\n"
    (toMs tBatch) (toUs tBatch / fromIntegral totalMetrics) batchSize
  printf "metric write speedup: %.2fx\n" (tSingle / tBatch)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest

benchMemorySize :: IO ()
benchMemorySize = do
  let fnCount = 4000 :: Int
      currentRounds = 200000 :: Int
      legacyRounds = 80 :: Int

  mem <- newPersist useMemory :: IO Memory

  putStrLn "[2/2] Seeding Memory backend for memorySize benchmark..."
  forM_ [1 .. fnCount] $ \i -> do
    let fn = FuncName (B.pack ("f-" ++ show i))
        jn = JobName "job"
        job = setSchedAt 1 (initJob fn jn)
    insert mem Pending job

  putStrLn "[2/2] Running memorySize benchmark..."

  tCurrent <- timeIt $ replicateM_ currentRounds $ do
    x <- memorySize mem
    _ <- evaluate x
    pure ()

  tLegacySim <- timeIt $ replicateM_ legacyRounds $ do
    x <- legacyMemorySize mem
    _ <- evaluate x
    pure ()

  printf "memorySize current: %.2f ms (%.2f us/op, rounds=%d)\n"
    (toMs tCurrent) (toUs tCurrent / fromIntegral currentRounds) currentRounds
  printf "memorySize legacy-simulated: %.2f ms (%.2f us/op, rounds=%d)\n"
    (toMs tLegacySim) (toUs tLegacySim / fromIntegral legacyRounds) legacyRounds
  printf "memorySize speedup (simulated): %.2fx\n"
    ((tLegacySim / fromIntegral legacyRounds) / (tCurrent / fromIntegral currentRounds))

legacyMemorySize :: Memory -> IO Int64
legacyMemorySize mem = do
  fns <- funcList mem
  sums <- mapM totalByFunc fns
  pure $ sum sums
  where
    totalByFunc fn = do
      a <- size mem Pending fn
      b <- size mem Running fn
      c <- size mem Locked fn
      pure $ a + b + c

timeIt :: IO a -> IO Double
timeIt action = do
  t0 <- getMonotonicTimeNSec
  _ <- action
  t1 <- getMonotonicTimeNSec
  pure $ fromIntegral (t1 - t0) / 1e9

toMs :: Double -> Double
toMs = (* 1000)

toUs :: Double -> Double
toUs = (* 1000000)
