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

newtype PSQL = PSQL Connection

instance Persist PSQL where
  data PersistConfig PSQL = PSQLPath ByteString
  data PersistException PSQL = PSQLException SomeException deriving (Show, Typeable)

  newPersist (PSQLPath path) = do
    infoM "Periodic.Server.Persist.PSQL" ("PSQL connected " ++ show path)
    conn <- connectPostgreSQL path
    withTransaction conn $ do
      createConfigTable conn
      createJobTable conn
      createFuncTable conn
      allPending conn
    return $ PSQL conn

  member           (PSQL conn) = doMember conn
  lookup           (PSQL conn) = doLookup conn
  insert           (PSQL conn) = doInsert conn
  delete           (PSQL conn) = doDelete conn
  size             (PSQL conn) = doSize conn
  foldr            (PSQL conn) = doFoldr conn
  foldr'           (PSQL conn) = doFoldr' conn
  configSet        (PSQL conn) = doConfigSet conn
  configGet        (PSQL conn) = doConfigGet conn
  insertFuncName   (PSQL conn) = doInsertFuncName conn
  removeFuncName   (PSQL conn) = doRemoveFuncName conn
  funcList         (PSQL conn) = doFuncList conn
  minSchedAt       (PSQL conn) = doMinSchedAt conn Pending
  transact         (PSQL conn) = doTransact conn
  transactReadOnly (PSQL conn) = doTransact conn

instance Exception (PersistException PSQL)

instance IsString (PersistConfig PSQL) where
  fromString = PSQLPath . fromString

doTransact :: Connection -> IO a -> IO a
doTransact _ = id

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

insertOrUpdate :: ToRow a => TableName -> Columns -> Columns -> a -> Connection -> IO Int64
insertOrUpdate tn ucols vcols a conn = execute conn sql a
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

update :: ToRow a => TableName -> Columns -> String -> a -> Connection -> IO Int64
update tn cols partSql a conn = execute conn sql a
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

selectOneOnly :: (ToRow a, FromRow (Only b), Show b) => TableName -> Column -> String -> a -> Connection -> IO (Maybe b)
selectOneOnly tn col partSql a conn =
  fmap fromOnly <$> selectOne tn [col] partSql a conn

count :: ToRow a => TableName -> String -> a -> Connection -> IO Int64
count tn partSql a conn =
  getOnlyDefault 0 <$> query conn sql a
  where whereSql = " WHERE " ++ partSql
        sql = fromString $ concat
          [ "SELECT count(*) FROM ", getTableName tn, whereSql
          ]

delete :: ToRow a => TableName -> String -> a -> Connection -> IO Int64
delete tn partSql a conn = execute conn sql a
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

allPending :: Connection -> IO ()
allPending = void . update jobs ["state"] "" (Only (stateName Pending))

doLookup :: Connection -> State -> FuncName -> JobName -> IO (Maybe Job)
doLookup conn state fn jn = do
  r <- selectOneOnly jobs "value" "func=? AND name=? AND state=?"
        (unFN fn, unJN jn, stateName state) conn
  case r of
    Nothing -> return Nothing
    Just bs ->
      case decodeJob bs of
        Left e -> do
          errorM "Periodic.Server.Persist.PSQL" $ "doLookup error: decode " ++ show bs ++ " " ++ show e
          return Nothing
        Right job -> return $ Just job

doMember :: Connection -> State -> FuncName -> JobName -> IO Bool
doMember conn st fn jn = isJust <$> doLookup conn st fn jn

doInsert :: Connection -> State -> FuncName -> JobName -> Job -> IO ()
doInsert conn state fn jn job = do
  doInsertFuncName conn fn
  void $ insertOrUpdate jobs
    ["func", "name"]
    ["value", "state", "sched_at"]
    (unFN fn, unJN jn, encode $ toBytes job, stateName state, getSchedAt job)
    conn

doInsertFuncName :: Connection -> FuncName -> IO ()
doInsertFuncName conn fn = void $ insertOrUpdate funcs ["func"] [] (Only $ unFN fn) conn

doFoldr :: Connection -> State -> (Job -> a -> a) -> a -> IO a
doFoldr conn state f acc =
  fold conn sql (Only $ stateName state) acc (mkFoldFunc f)
  where sql = fromString $ "SELECT value FROM " ++ getTableName jobs ++ " WHERE state=?"

doFoldr' :: Connection -> State -> [FuncName] -> (Job -> a -> a) -> a -> IO a
doFoldr' conn state fns f acc = F.foldrM (foldFunc f) acc fns

  where sql = fromString $ "SELECT value FROM " ++ getTableName jobs ++ " WHERE func=? AND state=?"
        foldFunc :: (Job -> a -> a) -> FuncName -> a -> IO a
        foldFunc  f0 fn acc0 =
          fold conn sql (unFN fn, stateName state) acc0 (mkFoldFunc f0)

doFuncList :: Connection -> IO [FuncName]
doFuncList conn =
  map (FuncName . fromOnly) <$> query_ conn sql
  where sql = fromString $ "SELECT func FROM " ++ getTableName funcs

doDelete :: Connection -> FuncName -> JobName -> IO ()
doDelete conn fn jn = void $ delete jobs "func=? AND name=?" (unFN fn, unJN jn) conn

doRemoveFuncName :: Connection -> FuncName -> IO ()
doRemoveFuncName conn fn = do
  void $ delete jobs "func=?" (Only $ unFN fn) conn
  void $ delete funcs "func=?" (Only $ unFN fn) conn

doMinSchedAt :: Connection -> State -> FuncName -> IO Int64
doMinSchedAt conn state fn =
  fromMaybe 0
    . fromMaybe Nothing
    <$> selectOneOnly jobs "min(sched_at)" "func=? AND state=?"
    (unFN fn, stateName state)
    conn

doSize :: Connection -> State -> FuncName -> IO Int64
doSize conn state fn = count jobs "func=? AND state=?" (unFN fn, stateName state) conn

doConfigSet :: Connection -> String -> Int -> IO ()
doConfigSet conn name v =
  void $ insertOrUpdate configs ["name"] ["value"] (name, v) conn

doConfigGet :: Connection -> String -> IO (Maybe Int)
doConfigGet conn name = selectOneOnly configs  "value" "name=?" (Only name) conn

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
