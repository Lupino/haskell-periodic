{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Periodic.Server.Persist.PSQL
  ( PSQL
  , usePSQL
  ) where


import           Control.Concurrent      (getNumCapabilities)
import           Control.Monad           (void)
import           Data.Binary             (decodeOrFail)
import           Data.Byteable           (toBytes)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B (init, length, tail)
import           Data.ByteString.Base64  (decode, encode)
import qualified Data.ByteString.Char8   as B (drop, split, take, unpack)
import qualified Data.ByteString.Lazy    as BL (fromStrict, null)
import           Data.Int                (Int64)
import           Data.List               (intercalate)
import           Data.Maybe              (fromMaybe)
import           Data.String             (IsString (..), fromString)
import           Database.PSQL           (From, FromField (..), Only (..),
                                          PSQLPool,
                                          ResultError (ConversionFailed), Size,
                                          TableName, TablePrefix, ToField (..),
                                          ToRow (..), constraintPrimaryKey,
                                          count, createIndex, createPSQLPool,
                                          createTable, execute, getTableName,
                                          getTablePrefix, insertOrUpdate,
                                          pageAsc, pageNone, query, returnError,
                                          runPSQLPool, selectOneOnly,
                                          selectOnly, selectOnly_, update,
                                          withTransaction)
import qualified Database.PSQL           as DB (PSQL)
import qualified Database.PSQL           as PSQL (delete)
import           Metro.Utils             (getEpochTime)
import           Periodic.Server.Persist (FuncStats (..), Persist (..),
                                          State (..))
import           Periodic.Types.Job      (FuncName (..), Job, JobName (..),
                                          getFuncName, getName, getSchedAt)
import           Prelude                 hiding (foldr, lookup)
import           System.Log.Logger       (infoM)
import           UnliftIO                (Exception, SomeException, Typeable)

instance ToField FuncName where
  toField (FuncName fn) = toField fn

instance FromField FuncName where
  fromField f dat = FuncName <$> fromField f dat

instance ToField JobName where
  toField (JobName jn) = toField jn

instance ToField State where
  toField Pending = toField (0 :: Int)
  toField Running = toField (1 :: Int)
  toField Locked  = toField (2 :: Int)

instance FromField Job where
  fromField f dat = do
    r <- fromField f dat
    case decodeJob r of
      Left e   -> returnError ConversionFailed f $ "decodeJob " ++ e
      Right ok -> pure ok

instance ToField Job where
  toField = toField . encode . toBytes

newtype MetricRows = MetricRows [(String, String, Int, Int64)]

instance ToRow MetricRows where
  toRow (MetricRows rows) = concatMap toRow rows

data PSQL = PSQL PSQLPool TablePrefix

idleTime = 10

maxMetricRowsPerInsert :: Int
maxMetricRowsPerInsert = 10000

runDB :: PSQL -> DB.PSQL a -> IO a
runDB (PSQL pool tablePrefix) = runPSQLPool tablePrefix pool

runDB_ :: PSQL -> DB.PSQL Int64 -> IO ()
runDB_ db = void . runDB db

preparePath :: ByteString -> (Maybe ByteString, ByteString)
preparePath = go . B.split ' '
  where go :: [ByteString] -> (Maybe ByteString, ByteString)
        go [] = (Nothing, "")
        go (x:xs)
          | B.take 7 x == "prefix=" = (Just (cleanPrefix $ B.drop 7 x), ys)
          | B.length ys == 0 = (y, x)
          | otherwise = (y, x <> " " <> ys)
          where (y, ys) = go xs

        cleanPrefix :: ByteString -> ByteString
        cleanPrefix bs
          | B.length bs >= 2 && B.take 1 bs == "'" && B.take 1 (B.drop (B.length bs - 1) bs) == "'" = B.init (B.tail bs)
          | B.length bs >= 2 && B.take 1 bs == "\"" && B.take 1 (B.drop (B.length bs - 1) bs) == "\"" = B.init (B.tail bs)
          | otherwise = bs

instance Persist PSQL where
  data PersistConfig PSQL = PSQLPath ByteString
  data PersistException PSQL = PSQLException SomeException deriving (Show, Typeable)

  newPersist (PSQLPath path) = do
    infoM "Periodic.Server.Persist.PSQL" ("PSQL connected " ++ show path)
    maxResources <- (*2) <$> getNumCapabilities
    pool <- createPSQLPool dbpath Nothing idleTime maxResources
    runPSQLPool tablePrefix pool $ withTransaction $ do
      void createConfigTable
      void createJobTable
      void createJobIndexes
      void createFuncTable
      void createMetricTable
      void allPending
    return $ PSQL pool tablePrefix

    where (mTablePrefix, dbpath) = preparePath path
          tablePrefix = fromString . B.unpack $ fromMaybe "" mTablePrefix

  getOne         db st fn = runDB  db . doGetOne st fn
  insert         db st    = runDB_ db . doInsert st
  updateState    db st fn = runDB_ db . doUpdateState st fn
  delete         db fn    = runDB_ db . doDelete fn
  insertPendingUnlessRunning db = runDB db . doInsertPendingUnlessRunning
  size           db st    = runDB  db . doSize st
  getRunningJob  db       = runDB  db . doGetRunningJob
  getPendingJob  db f t c
    | c <= 0    = pure []
    | otherwise = runDB db (doGetPendingJob f t 0 (fromIntegral c))
  getLockedJob   db fn    = runDB  db . doGetLockedJob fn
  dumpJob        db       = runDB  db   doDumpJob
  configSet      db name  = runDB_ db . doConfigSet name
  configGet      db       = runDB  db . doConfigGet
  insertFuncName db       = runDB_ db . doInsertFuncName
  removeFuncName db       = runDB  db . doRemoveFuncName
  funcList       db       = runDB  db   doFuncList
  minSchedAt     db       = runDB  db . doMinSchedAt Pending
  getFuncStats   db       = runDB  db . doGetFuncStats
  getAllFuncStats db      = runDB  db   doGetAllFuncStats
  countPending   db ts    = runDB  db . doCountPending ts
  insertMetric   db e n   = runDB  db . doInsertMetric e n
  insertMetrics  db       = runDB  db . doInsertMetrics

instance Exception (PersistException PSQL)

instance IsString (PersistConfig PSQL) where
  fromString = usePSQL

usePSQL :: String -> PersistConfig PSQL
usePSQL = PSQLPath . fromString

configs :: TableName
configs = "configs"

jobs :: TableName
jobs = "jobs"

funcs :: TableName
funcs = "funcs"

metrics :: TableName
metrics = "metrics"

createConfigTable :: DB.PSQL Int64
createConfigTable = do
  tablePrefix <- getTablePrefix
  createTable configs
    [ "name VARCHAR(256) NOT NULL"
    , "value INT DEFAULT 0"
    , constraintPrimaryKey tablePrefix configs ["name"]
    ]

createJobTable :: DB.PSQL Int64
createJobTable = do
  tablePrefix <- getTablePrefix
  createTable jobs
    [ "func VARCHAR(256) NOT NULL"
    , "name VARCHAR(256) NOT NULL"
    , "value text"
    , "state INT DEFAULT 0"
    , "sched_at INT DEFAULT 0"
    , constraintPrimaryKey tablePrefix jobs ["func", "name"]
    ]

createJobIndexes :: DB.PSQL Int64
createJobIndexes = do
  _ <- createIndex False jobs "idx_jobs_func_state_sched_at"
       ["func", "state", "sched_at"]
  createIndex False jobs "idx_jobs_state_sched_at"
    ["state", "sched_at"]

createFuncTable :: DB.PSQL Int64
createFuncTable = do
  tablePrefix <- getTablePrefix
  createTable funcs
    [ "func VARCHAR(256) NOT NULL"
    , constraintPrimaryKey tablePrefix funcs ["func"]
    ]


createMetricTable :: DB.PSQL Int64
createMetricTable =
  createTable metrics
    [ "id BIGSERIAL PRIMARY KEY"
    , "event VARCHAR(50) NOT NULL"
    , "name VARCHAR(150) NOT NULL"
    , "duration_ms INT DEFAULT '0'"
    , "created_at INT DEFAULT '0'"
    ]

allPending :: DB.PSQL Int64
allPending = update jobs ["state"] "" (Only Pending)

doGetOne :: State -> FuncName -> JobName -> DB.PSQL (Maybe Job)
doGetOne state fn jn = do
  selectOneOnly jobs "value" "func=? AND name=? AND state=?"
    (fn, jn, state)

doInsert :: State -> Job -> DB.PSQL Int64
doInsert state job = do
  void $ doInsertFuncName fn
  insertOrUpdate jobs
    ["func", "name"]
    ["value", "state", "sched_at"]
    []
    (fn, jn, job, state, getSchedAt job)
  where fn = getFuncName job
        jn = getName     job

doInsertPendingUnlessRunning :: Job -> DB.PSQL Bool
doInsertPendingUnlessRunning job = withTransaction $ do
  running <- doGetOne Running fn jn
  case running of
    Just _  -> pure False
    Nothing -> doInsert Pending job >> pure True
  where fn = getFuncName job
        jn = getName     job

doUpdateState :: State -> FuncName -> JobName -> DB.PSQL Int64
doUpdateState state fn jn =
  update jobs ["state"] "func=? AND name=?" (state, fn, jn)

doInsertFuncName :: FuncName -> DB.PSQL Int64
doInsertFuncName fn = insertOrUpdate funcs ["func"] [] [] (Only fn)

doGetRunningJob :: Int64 -> DB.PSQL [Job]
doGetRunningJob ts =
  selectOnly jobs "value" "state=? AND sched_at < ?" (Running, ts) (pageAsc 0 1000 "sched_at")

doGetPendingJob :: FuncName -> Int64 -> From -> Size -> DB.PSQL [Job]
doGetPendingJob fn ts f s =
  selectOnly jobs "value" "func =? AND state=? AND sched_at < ?"
  (fn, Pending, ts) (pageAsc f s "sched_at")

doGetLockedJob :: FuncName -> Int -> DB.PSQL [Job]
doGetLockedJob fn c = do
  selectOnly jobs "value" "func=? AND state=?" (fn, Locked) (pageAsc 0 s "sched_at")

  where s = fromIntegral c

doCountPending :: FuncName -> Int64 -> DB.PSQL Int
doCountPending fn ts =
  fromIntegral <$> count jobs "func =? AND state=? AND sched_at < ?"
      (fn, Pending, ts)

doDumpJob :: DB.PSQL [Job]
doDumpJob = selectOnly_ jobs "value" pageNone

doFuncList :: DB.PSQL [FuncName]
doFuncList = map FuncName <$> selectOnly_ funcs "func" pageNone

doDelete :: FuncName -> JobName -> DB.PSQL Int64
doDelete fn jn = PSQL.delete jobs "func=? AND name=?" (fn, jn)

doRemoveFuncName :: FuncName -> DB.PSQL ()
doRemoveFuncName fn = do
  void $ PSQL.delete jobs "func=?" (Only fn)
  void $ PSQL.delete funcs "func=?" (Only fn)

doMinSchedAt :: State -> FuncName -> DB.PSQL Int64
doMinSchedAt state fn =
  fromMaybe 0
    . fromMaybe Nothing
    <$> selectOneOnly jobs "min(sched_at)" "func=? AND state=?"
    (fn, state)

doSize :: State -> FuncName -> DB.PSQL Int64
doSize state fn = count jobs "func=? AND state=?" (fn, state)

doGetFuncStats :: FuncName -> DB.PSQL FuncStats
doGetFuncStats fn = do
  rows <- query mkQuery (fn, fn, fn, fn) :: DB.PSQL [(Int64, Int64, Int64, Maybe Int64)]
  case rows of
    [(pending, running, locked, schedAt)] ->
      pure FuncStats
        { funcPending = pending
        , funcRunning = running
        , funcLocked = locked
        , funcSchedAt = fromMaybe 0 schedAt
        }
    _ -> pure $ FuncStats 0 0 0 0
  where
    mkQuery tablePrefix = fromString $
      "SELECT"
      ++ " (SELECT count(*) FROM " ++ tableName ++ " WHERE func=? AND state=0),"
      ++ " (SELECT count(*) FROM " ++ tableName ++ " WHERE func=? AND state=1),"
      ++ " (SELECT count(*) FROM " ++ tableName ++ " WHERE func=? AND state=2),"
      ++ " (SELECT sched_at FROM " ++ tableName ++ " WHERE func=? AND state=0 ORDER BY sched_at ASC LIMIT 1)"
      where tableName = getTableName tablePrefix jobs

doGetAllFuncStats :: DB.PSQL [(FuncName, FuncStats)]
doGetAllFuncStats = do
  rows <- query mkQuery () :: DB.PSQL [(FuncName, Int64, Int64, Int64, Maybe Int64)]
  pure
    [ (fn, FuncStats pending running locked (fromMaybe 0 schedAt))
    | (fn, pending, running, locked, schedAt) <- rows
    ]
  where
    mkQuery tablePrefix = fromString $
      "SELECT funcs.func,"
      ++ " SUM(CASE WHEN jobs.state=0 THEN 1 ELSE 0 END),"
      ++ " SUM(CASE WHEN jobs.state=1 THEN 1 ELSE 0 END),"
      ++ " SUM(CASE WHEN jobs.state=2 THEN 1 ELSE 0 END),"
      ++ " MIN(CASE WHEN jobs.state=0 THEN jobs.sched_at END)"
      ++ " FROM " ++ getTableName tablePrefix funcs
      ++ " AS funcs LEFT JOIN " ++ getTableName tablePrefix jobs
      ++ " AS jobs ON funcs.func=jobs.func GROUP BY funcs.func"

doConfigSet :: String -> Int -> DB.PSQL Int64
doConfigSet name v =
  insertOrUpdate configs ["name"] ["value"] [] (name, v)

doConfigGet :: String -> DB.PSQL (Maybe Int)
doConfigGet name = selectOneOnly configs  "value" "name=?" (Only name)

decodeJob :: ByteString -> Either String Job
decodeJob bs =
  case decode bs of
    Left e -> Left e
    Right bs0 ->
      case decodeOrFail (BL.fromStrict bs0) of
        Left e            -> Left $ show e
        Right (rest, _, job)
          | BL.null rest -> Right job
          | otherwise    -> Left "decodeJob trailing bytes"

doInsertMetric :: String -> String -> Int -> DB.PSQL ()
doInsertMetric event name durationMs =
  doInsertMetrics [(event, name, durationMs)]

doInsertMetrics :: [(String, String, Int)] -> DB.PSQL ()
doInsertMetrics [] = pure ()
doInsertMetrics items = do
  now <- getEpochTime
  mapM_ (doInsertMetricBatch now) $ chunksOf maxMetricRowsPerInsert items

doInsertMetricBatch :: Int64 -> [(String, String, Int)] -> DB.PSQL ()
doInsertMetricBatch now items =
  void $ execute (mkQuery $ length items) $ MetricRows
    [ (event, name, durationMs, now)
    | (event, name, durationMs) <- items
    ]
  where
    mkQuery rowCount tablePrefix = fromString $
      "INSERT INTO " ++ getTableName tablePrefix metrics
      ++ " (event, name, duration_ms, created_at) VALUES "
      ++ intercalate ", " (replicate rowCount "(?, ?, ?, ?)")

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest
