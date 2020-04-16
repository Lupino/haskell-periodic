{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Periodic.Server.Persist.PSQL
  ( PSQL
  ) where

import           Control.Monad              (void)
import           Data.Binary                (decodeOrFail)
import           Data.Byteable              (toBytes)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Base64     (decode, encode)
import           Data.ByteString.Lazy       (fromStrict)
import qualified Data.Foldable              as F (foldrM)
import           Data.Int                   (Int64)
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromMaybe, isJust, listToMaybe)
import           Data.Pool                  (Pool, createPool, withResource)
import           Data.String                (IsString (..))
import           Database.PostgreSQL.Simple
import           Periodic.Server.Persist    (Persist (PersistConfig, PersistException),
                                             State (..))
import qualified Periodic.Server.Persist    as Persist
import           Periodic.Types.Job         (FuncName (..), Job, JobName (..),
                                             getSchedAt)
import           Prelude                    hiding (foldr, lookup)
import           System.Log.Logger          (errorM, infoM)
import           UnliftIO                   (Exception, SomeException, Typeable)

stateName :: State -> ByteString
stateName Pending = "0"
stateName Running = "1"
stateName Locking = "2"

newtype PSQL = PSQL (Pool Connection)

numStripes = 1
idleTime = 10
maxResources = 10

instance Persist PSQL where
  data PersistConfig PSQL = PSQLPath ByteString
  data PersistException PSQL = PSQLException SomeException deriving (Show, Typeable)

  newPersist (PSQLPath path) = do
    infoM "Periodic.Server.Persist.PSQL" ("PSQL connected " ++ show path)
    pool <- createPool (connectPostgreSQL path) close numStripes idleTime maxResources
    withResource pool $ \conn ->
      withTransaction conn $ do
        createConfigTable conn
        createJobTable conn
        createFuncTable conn
    allPending pool
    return $ PSQL pool

  member         (PSQL pool) = doMember pool
  lookup         (PSQL pool) = doLookup pool
  insert         (PSQL pool) = doInsert pool
  delete         (PSQL pool) = doDelete pool
  size           (PSQL pool) = doSize pool
  foldr          (PSQL pool) = doFoldr pool
  foldrPending   (PSQL pool) = doFoldrPending pool
  dumpJob        (PSQL pool) = doDumpJob pool
  configSet      (PSQL pool) = doConfigSet pool
  configGet      (PSQL pool) = doConfigGet pool
  insertFuncName (PSQL pool) = doInsertFuncName pool
  removeFuncName (PSQL pool) = doRemoveFuncName pool
  funcList       (PSQL pool) = doFuncList pool
  minSchedAt     (PSQL pool) = doMinSchedAt pool Pending

instance Exception (PersistException PSQL)

instance IsString (PersistConfig PSQL) where
  fromString = PSQLPath . fromString

newtype TableName = TableName String
  deriving (Show)

instance IsString TableName where
  fromString = TableName

getTableName :: TableName -> String
getTableName (TableName name) = name

newtype Column = Column { unColumn :: String }
  deriving (Show)

instance IsString Column where
  fromString = Column

type Columns = [Column]

columnsToString :: Columns -> String
columnsToString = intercalate ", " . map unColumn

createTable :: TableName -> Columns -> Connection -> IO Int64
createTable tn cols conn = execute_ conn sql
  where sql = fromString $ concat
          [ "CREATE TABLE IF NOT EXISTS ", getTableName tn, " ("
          , columnsToString cols
          , ")"
          ]

newtype IndexName = IndexName String
  deriving (Show)

instance IsString IndexName where
  fromString = IndexName

getOnlyDefault :: FromRow (Only a) => a -> [Only a] -> a
getOnlyDefault a = maybe a fromOnly . listToMaybe

insertOrUpdate :: ToRow a => TableName -> Columns -> Columns -> a -> Pool Connection -> IO Int64
insertOrUpdate tn ucols vcols a pool = withResource pool $ \conn -> execute conn sql a
  where cols = ucols ++ vcols
        v = replicate (length cols) "?"

        setSql = intercalate ", " $ map appendSet vcols

        appendSet :: Column -> String
        appendSet (Column col) | '=' `elem` col = col
                               | otherwise = col ++ " = excluded." ++ col

        doSql = if null vcols then " DO NOTHING" else " DO UPDATE SET " ++ setSql

        sql = fromString $ concat
          [ "INSERT INTO ", getTableName tn
          , " (", columnsToString cols, ")"
          , " VALUES"
          , " (", columnsToString v, ")"
          , " ON CONFLICT (", columnsToString ucols, ")"
          , doSql
          ]

update :: ToRow a => TableName -> Columns -> String -> a -> Pool Connection -> IO Int64
update tn cols partSql a pool = withResource pool $ \conn -> execute conn sql a
  where setSql = intercalate ", " $ map appendSet cols
        whereSql = if null partSql then "" else " WHERE " ++ partSql
        sql = fromString $ concat
          [ "UPDATE ", getTableName tn
          , " SET ", setSql
          , whereSql
          ]

        appendSet :: Column -> String
        appendSet (Column col) | '=' `elem` col = col
                               | otherwise = col ++ " = ?"


selectOne :: (ToRow a, FromRow b, Show b) => TableName -> Columns -> String -> a -> Connection -> IO (Maybe b)
selectOne tn cols partSql a conn = listToMaybe <$> query conn sql a
  where whereSql = " WHERE " ++ partSql
        sql = fromString $ concat
          [ "SELECT ", columnsToString cols, " FROM ", getTableName tn
          , whereSql
          ]

selectOneOnly
  :: (ToRow a, FromRow (Only b), Show b)
  => TableName -> Column -> String -> a -> Pool Connection -> IO (Maybe b)
selectOneOnly tn col partSql a pool = withResource pool $ \conn ->
  fmap fromOnly <$> selectOne tn [col] partSql a conn

count :: ToRow a => TableName -> String -> a -> Pool Connection -> IO Int64
count tn partSql a pool = withResource pool $ \conn ->
  getOnlyDefault 0 <$> query conn sql a
  where whereSql = " WHERE " ++ partSql
        sql = fromString $ concat
          [ "SELECT count(*) FROM ", getTableName tn, whereSql
          ]

delete :: ToRow a => TableName -> String -> a -> Pool Connection -> IO Int64
delete tn partSql a pool = withResource pool $ \conn -> execute conn sql a
  where whereSql = " WHERE " ++ partSql
        sql = fromString $ concat
          [ "DELETE FROM ", getTableName tn, whereSql
          ]


configs :: TableName
configs = "configs"

jobs :: TableName
jobs = "jobs"

funcs :: TableName
funcs = "funcs"

createConfigTable :: Connection -> IO ()
createConfigTable =
  void . createTable configs
    [ "name VARCHAR(256) NOT NULL"
    , "value INT DEFAULT 0"
    , "CONSTRAINT config_pk PRIMARY KEY (name)"
    ]

createJobTable :: Connection -> IO ()
createJobTable =
  void . createTable jobs
    [ "func VARCHAR(256) NOT NULL"
    , "name VARCHAR(256) NOT NULL"
    , "value text"
    , "state INT DEFAULT 0"
    , "sched_at INT DEFAULT 0"
    , "CONSTRAINT job_pk PRIMARY KEY (func, name)"
    ]

createFuncTable :: Connection -> IO ()
createFuncTable =
  void . createTable funcs
    [ "func VARCHAR(256) NOT NULL"
    , "CONSTRAINT func_pk PRIMARY KEY (func)"
    ]

allPending :: Pool Connection -> IO ()
allPending = void . update jobs ["state"] "" (Only (stateName Pending))

doLookup :: Pool Connection -> State -> FuncName -> JobName -> IO (Maybe Job)
doLookup pool state fn jn = do
  r <- selectOneOnly jobs "value" "func=? AND name=? AND state=?"
        (unFN fn, unJN jn, stateName state) pool
  case r of
    Nothing -> return Nothing
    Just bs ->
      case decodeJob bs of
        Left e -> do
          errorM "Periodic.Server.Persist.PSQL" $ "doLookup error: decode " ++ show bs ++ " " ++ show e
          return Nothing
        Right job -> return $ Just job

doMember :: Pool Connection -> State -> FuncName -> JobName -> IO Bool
doMember pool st fn jn = isJust <$> doLookup pool st fn jn

doInsert :: Pool Connection -> State -> FuncName -> JobName -> Job -> IO ()
doInsert pool state fn jn job = do
  doInsertFuncName pool fn
  void $ insertOrUpdate jobs
    ["func", "name"]
    ["value", "state", "sched_at"]
    (unFN fn, unJN jn, encode $ toBytes job, stateName state, getSchedAt job)
    pool

doInsertFuncName :: Pool Connection -> FuncName -> IO ()
doInsertFuncName pool fn = void $ insertOrUpdate funcs ["func"] [] (Only $ unFN fn) pool

doFoldr :: Pool Connection -> State -> (Job -> a -> a) -> a -> IO a
doFoldr pool state f acc = withResource pool $ \conn ->
  fold conn sql (Only $ stateName state) acc (mkFoldFunc f)
  where sql = fromString $ "SELECT value FROM " ++ getTableName jobs ++ " WHERE state=?"

doFoldrPending :: Pool Connection -> Int64 -> [FuncName] -> (Job -> a -> a) -> a -> IO a
doFoldrPending pool ts fns f acc = withResource pool $ \conn ->
  F.foldrM (foldFunc conn f) acc fns

  where sql = fromString $ "SELECT value FROM " ++ getTableName jobs ++ " WHERE func=? AND state=? AND sched_at < ?"
        foldFunc :: Connection -> (Job -> a -> a) -> FuncName -> a -> IO a
        foldFunc conn f0 fn acc0 =
          fold conn sql (unFN fn, stateName Pending, ts) acc0 (mkFoldFunc f0)

doDumpJob :: Pool Connection -> IO [Job]
doDumpJob pool = withResource pool $ \conn ->
  fold_ conn sql [] (mkFoldFunc (:))
  where sql = fromString $ "SELECT value FROM " ++ getTableName jobs

doFuncList :: Pool Connection -> IO [FuncName]
doFuncList pool = withResource pool $ \conn ->
  map (FuncName . fromOnly) <$> query_ conn sql
  where sql = fromString $ "SELECT func FROM " ++ getTableName funcs

doDelete :: Pool Connection -> FuncName -> JobName -> IO ()
doDelete pool fn jn = void $ delete jobs "func=? AND name=?" (unFN fn, unJN jn) pool

doRemoveFuncName :: Pool Connection -> FuncName -> IO ()
doRemoveFuncName pool fn = do
  void $ delete jobs "func=?" (Only $ unFN fn) pool
  void $ delete funcs "func=?" (Only $ unFN fn) pool

doMinSchedAt :: Pool Connection -> State -> FuncName -> IO Int64
doMinSchedAt pool state fn =
  fromMaybe 0
    . fromMaybe Nothing
    <$> selectOneOnly jobs "min(sched_at)" "func=? AND state=?"
    (unFN fn, stateName state)
    pool

doSize :: Pool Connection -> State -> FuncName -> IO Int64
doSize pool state fn = count jobs "func=? AND state=?" (unFN fn, stateName state) pool

doConfigSet :: Pool Connection -> String -> Int -> IO ()
doConfigSet pool name v =
  void $ insertOrUpdate configs ["name"] ["value"] (name, v) pool

doConfigGet :: Pool Connection -> String -> IO (Maybe Int)
doConfigGet pool name = selectOneOnly configs  "value" "name=?" (Only name) pool

mkFoldFunc :: (Job -> a -> a) -> a -> Only ByteString -> IO a
mkFoldFunc f acc (Only bs) =
  case decodeJob bs of
    Left e -> do
      errorM "Periodic.Server.Persist.PSQL" $ "mkFoldFunc error: decode " ++ show bs ++ " " ++ show e
      return acc
    Right job -> return $ f job acc

decodeJob :: ByteString -> Either String Job
decodeJob bs =
  case decode bs of
    Left e -> Left e
    Right bs0 ->
      case decodeOrFail (fromStrict bs0) of
        Left e            -> Left $ show e
        Right (_, _, job) -> Right job
