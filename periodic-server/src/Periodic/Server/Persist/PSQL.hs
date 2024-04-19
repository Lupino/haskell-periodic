{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Periodic.Server.Persist.PSQL
  ( PSQL
  , usePSQL
  ) where


import           Control.Monad           (void)
import           Data.Binary             (decodeOrFail)
import           Data.Byteable           (toBytes)
import           Data.ByteString         (ByteString)
import           Data.ByteString.Base64  (decode, encode)
import           Data.ByteString.Lazy    (fromStrict)
import           Data.Int                (Int64)
import           Data.Maybe              (fromMaybe)
import           Data.String             (IsString (..))
import           Database.PSQL.Types     (From (..), FromField (..), Only (..),
                                          PSQLPool, Size (..), TableName,
                                          ToField (..), asc, count,
                                          createPSQLPool, createTable, delete,
                                          insertOrUpdate, none, runPSQLPool,
                                          selectOneOnly, selectOnly,
                                          selectOnly_, update, withTransaction)
import qualified Database.PSQL.Types     as DB (PSQL)
import           Periodic.Server.Persist (Persist (PersistConfig, PersistException),
                                          State (..), loopFetchData)
import qualified Periodic.Server.Persist as Persist
import           Periodic.Types.Job      (FuncName (..), Job, JobName (..),
                                          getFuncName, getName, getSchedAt)
import           Prelude                 hiding (foldr, lookup)
import           System.Log.Logger       (infoM)
import           UnliftIO                (Exception, SomeException, Typeable)

instance ToField FuncName where
  toField (FuncName fn) = toField fn

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
      Left e   -> error $ "decodeJob " ++ e
      Right ok -> pure ok

instance ToField Job where
  toField = toField . encode . toBytes

newtype PSQL = PSQL PSQLPool

idleTime = 10
maxResources = 10

runDB :: PSQL -> DB.PSQL a -> IO a
runDB (PSQL pool) = runPSQLPool "" pool

runDB_ :: PSQL -> DB.PSQL Int64 -> IO ()
runDB_ db = void . runDB db

instance Persist PSQL where
  data PersistConfig PSQL = PSQLPath ByteString
  data PersistException PSQL = PSQLException SomeException deriving (Show, Typeable)

  newPersist (PSQLPath path) = do
    infoM "Periodic.Server.Persist.PSQL" ("PSQL connected " ++ show path)
    pool <- createPSQLPool path idleTime maxResources
    runPSQLPool "" pool $ withTransaction $ do
      void createConfigTable
      void createJobTable
      void createFuncTable
      void allPending
    return $ PSQL pool

  getOne         db st fn = runDB  db . doGetOne st fn
  insert         db st    = runDB_ db . doInsert st
  updateState    db st fn = runDB_ db . doUpdateState st fn
  delete         db fn    = runDB_ db . doDelete fn
  size           db st    = runDB  db . doSize st
  getRunningJob  db       = runDB  db . doGetRunningJob
  getPendingJob  db f t c = loopFetchData 0 c 100 $ \offset ->
                              runDB db (doGetPendingJob f t (fromIntegral offset) 100)
  getLockedJob   db fn    = runDB  db . doGetLockedJob fn
  dumpJob        db       = runDB  db   doDumpJob
  configSet      db name  = runDB_ db . doConfigSet name
  configGet      db       = runDB  db . doConfigGet
  insertFuncName db       = runDB_ db . doInsertFuncName
  removeFuncName db       = runDB  db . doRemoveFuncName
  funcList       db       = runDB  db   doFuncList
  minSchedAt     db       = runDB  db . doMinSchedAt Pending
  countPending   db ts    = runDB  db . doCountPending ts

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

createConfigTable :: DB.PSQL Int64
createConfigTable =
  createTable configs
    [ "name VARCHAR(256) NOT NULL"
    , "value INT DEFAULT 0"
    , "CONSTRAINT config_pk PRIMARY KEY (name)"
    ]

createJobTable :: DB.PSQL Int64
createJobTable =
  createTable jobs
    [ "func VARCHAR(256) NOT NULL"
    , "name VARCHAR(256) NOT NULL"
    , "value text"
    , "state INT DEFAULT 0"
    , "sched_at INT DEFAULT 0"
    , "CONSTRAINT job_pk PRIMARY KEY (func, name)"
    ]

createFuncTable :: DB.PSQL Int64
createFuncTable =
  createTable funcs
    [ "func VARCHAR(256) NOT NULL"
    , "CONSTRAINT func_pk PRIMARY KEY (func)"
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

doUpdateState :: State -> FuncName -> JobName -> DB.PSQL Int64
doUpdateState state fn jn =
  update jobs ["state"] "func=? AND name=?" (state, fn, jn)

doInsertFuncName :: FuncName -> DB.PSQL Int64
doInsertFuncName fn = insertOrUpdate funcs ["func"] [] [] (Only fn)

doGetRunningJob :: Int64 -> DB.PSQL [Job]
doGetRunningJob ts =
  selectOnly jobs "value" "sched_at < ?" (Only ts) 0 1000 (asc "sched_at")

doGetPendingJob :: FuncName -> Int64 -> From -> Size -> DB.PSQL [Job]
doGetPendingJob fn ts f s =
  selectOnly jobs "value" "func =? AND state=? AND sched_at < ?"
      (fn, Pending, ts) f s (asc "sched_at")

doGetLockedJob :: FuncName -> Int -> DB.PSQL [Job]
doGetLockedJob fn c = do
  selectOnly jobs "value" "func=? AND state=?"
    (fn, Locked) 0 (Size $ fromIntegral c) (asc "sched_at")

doCountPending :: FuncName -> Int64 -> DB.PSQL Int
doCountPending fn ts =
  fromIntegral <$> count jobs "func =? AND state=? AND sched_at < ?"
      (fn, Pending, ts)

doDumpJob :: DB.PSQL [Job]
doDumpJob = selectOnly_ jobs "value" 0 10000 none

doFuncList :: DB.PSQL [FuncName]
doFuncList = map FuncName <$> selectOnly_ funcs "func" 0 10000 none

doDelete :: FuncName -> JobName -> DB.PSQL Int64
doDelete fn jn = delete jobs "func=? AND name=?" (fn, jn)

doRemoveFuncName :: FuncName -> DB.PSQL ()
doRemoveFuncName fn = do
  void $ delete jobs "func=?" (Only fn)
  void $ delete funcs "func=?" (Only fn)

doMinSchedAt :: State -> FuncName -> DB.PSQL Int64
doMinSchedAt state fn =
  fromMaybe 0
    . fromMaybe Nothing
    <$> selectOneOnly jobs "min(sched_at)" "func=? AND state=?"
    (fn, state)

doSize :: State -> FuncName -> DB.PSQL Int64
doSize state fn = count jobs "func=? AND state=?" (fn, state)

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
      case decodeOrFail (fromStrict bs0) of
        Left e            -> Left $ show e
        Right (_, _, job) -> Right job
