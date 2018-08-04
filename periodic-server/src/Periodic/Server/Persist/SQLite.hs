{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server.Persist.SQLite
  ( initSQLite
  ) where

import           Prelude                 hiding (foldr, lookup)

import           Control.Monad           (void)
import           Control.Monad.Catch
import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString)
import qualified Data.Foldable           as F (foldrM)
import           Data.Int                (Int64)
import           Data.Maybe              (isJust, listToMaybe)
import           Database.SQLite3.Direct
import           Periodic.Server.Persist (Persist (..), Persister (..))
import           Periodic.Types.Internal (runParser)
import           Periodic.Types.Job      (FuncName (..), Job (jSchedAt))
import           System.Log.Logger       (errorM)

newtype Table = Table ByteString

mainTableName :: Table
mainTableName = Table "main"

procTableName :: Table
procTableName = Table "proc"

initSQLite :: Utf8 -> IO Persist
initSQLite path = do
  edb <- open path
  case edb of
    Left (e, _) -> dbError $ show e
    Right db -> do
      beginTx db
      createTable db mainTableName
      createTable db procTableName
      createFuncTable db
      commitTx db
      pure $ Persist
        { main = Persister
          { member = doMember db mainTableName
          , lookup = doLookup db mainTableName
          , insert = doInsert db mainTableName
          , delete = doDelete db mainTableName
          , size = doSize db mainTableName
          , foldr = doFoldr db mainTableName
          , foldr' = doFoldr' db mainTableName
          }
        , proc = Persister
          { member = doMember db procTableName
          , lookup = doLookup db procTableName
          , insert = doInsert db procTableName
          , delete = doDelete db procTableName
          , size = doSize db procTableName
          , foldr = doFoldr db procTableName
          , foldr' = doFoldr' db procTableName
          }

        , insertFuncName = doInsertFuncName db
        , removeFuncName = doRemoveFuncName db
        , funcList = doFuncList db
        , minSchedAt = doMinSchedAt db mainTableName
        , transact = doTransact db
        , transactReadOnly = doTransact db
        }


beginTx :: Database -> IO ()
beginTx db = void $ exec db "BEGIN TRANSACTION"

commitTx :: Database -> IO ()
commitTx db = void $ exec db "COMMIT TRANSACTION"

rollbackTx :: Database -> IO ()
rollbackTx db = void $ exec db "ROLLBACK TRANSACTION"

doTransact :: Database -> IO a -> IO a
doTransact db io = do
  beginTx db
  r <- try io
  case r of
    Left (e :: SomeException) -> do
      errorM "Periodic.Server.Persist.SQLite" $ show e
      rollbackTx db
      throwM e
    Right v -> do
      commitTx db
      pure v

createTable :: Database -> Table -> IO ()
createTable db (Table tn) = void . exec db $ Utf8 $
  "CREATE TABLE IF NOT EXISTS " <> tn <> " ("
    <> "func CHAR(256) NOT NULL,"
    <> " name CHAR(256) NOT NULL,"
    <> " value BLOB,"
    <> " sched_at INTEGER DEFAULT 0,"
    <> " PRIMARY KEY (func, name))"

createFuncTable :: Database -> IO ()
createFuncTable db = void . exec db $ Utf8 $
  "CREATE TABLE IF NOT EXISTS funcs ("
    <> "func CHAR(256) NOT NULL,"
    <> " PRIMARY KEY (func))"

doLookup :: Byteable k => Database -> Table -> FuncName -> k -> IO (Maybe Job)
doLookup db (Table tn) fn jn =
  listToMaybe <$> doFoldr_ db sql (bindFnAndJn fn jn) (mkFoldFunc f) []
  where sql = Utf8 $ "SELECT value FROM " <> tn <> " WHERE func=? AND name=? LIMIT 1"
        f :: Job -> [Job] -> [Job]
        f job acc = job : acc

doMember :: Byteable k => Database -> Table -> FuncName -> k -> IO Bool
doMember db tb fn jn = isJust <$> doLookup db tb fn jn

doInsert :: Byteable k => Database -> Table -> FuncName -> k -> Job -> IO ()
doInsert db (Table tn) fn jn job = do
  execStmt db sql $ \stmt -> do
    bindFnAndJn fn jn stmt
    void $ bindBlob stmt 3 $ toBytes job
    void $ bindInt64 stmt 4 $ jSchedAt job
  doInsertFuncName db fn
  where sql = Utf8 $ "INSERT OR REPLACE INTO " <> tn <> " VALUES (?, ?, ?, ?)"

doInsertFuncName :: Database -> FuncName -> IO ()
doInsertFuncName db = execFN db sql
  where sql = Utf8 "INSERT OR REPLACE INTO funcs VALUES (?)"

doFoldr :: Database -> Table -> (Job -> a -> a) -> a -> IO a
doFoldr db (Table tn) f = doFoldr_ db sql (const $ pure ()) (mkFoldFunc f)
  where sql = Utf8 $ "SELECT value FROM " <> tn

doFoldr' :: Database -> Table -> [FuncName] -> (Job -> a -> a) -> a -> IO a
doFoldr' db (Table tn) fns f acc = F.foldrM (foldFunc f) acc fns
  where sql = Utf8 $ "SELECT value FROM " <> tn <> " WHERE func=?"

        foldFunc :: (Job -> a -> a) -> FuncName -> a -> IO a
        foldFunc  f0 fn =
          doFoldr_ db sql (`bindFN` fn) (mkFoldFunc f0)

doFuncList :: Database -> IO [FuncName]
doFuncList db =
  doFoldr_ db sql (const $ pure ()) (\fn acc -> FuncName fn : acc) []
  where sql = Utf8 "SELECT func FROM funcs"

doDelete :: Byteable k => Database -> Table -> FuncName -> k -> IO ()
doDelete db (Table tn) fn jn = execStmt db sql $ bindFnAndJn fn jn
  where sql = Utf8 $ "DELETE FROM " <> tn <> " WHERE func=? and name=?"

doRemoveFuncName :: Database -> FuncName -> IO ()
doRemoveFuncName db fn = do
  execFN db sql0 fn
  execFN db sql1 fn

  where sql0 = Utf8 "DELETE FROM funcs WHERE func=?"
        sql1 = Utf8 "DELETE FROM main WHERE func=?"

doMinSchedAt :: Database -> Table -> FuncName -> IO Int64
doMinSchedAt db (Table tn) fn = queryStmt db sql (`bindFN` fn) stepInt64
  where sql = Utf8 $ "SELECT sched_at FROM " <> tn <> " WHERE func=? ORDER BY sched_at ASC LIMIT 1"

doSize :: Database -> Table -> FuncName -> IO Int64
doSize db (Table tn) fn = queryStmt db sql (`bindFN` fn) stepInt64
  where sql = Utf8 $ "SELECT COUNT(*) FROM " <> tn <> " WHERE func=?"


dbError :: String -> IO a
dbError = throwM . userError . ("Database error: " ++)

liftEither :: Show a => IO (Either a b) -> IO b
liftEither a = do
  er <- a
  case er of
    (Left e)  -> dbError (show e)
    (Right r) -> return r
{-# INLINE liftEither #-}

prepStmt :: Database -> Utf8 -> IO Statement
prepStmt c q = do
    r <- prepare c q
    case r of
      Left e         -> dbError (show e)
      Right Nothing  -> dbError "Statement prep failed"
      Right (Just s) -> return s

bindFN :: Statement -> FuncName -> IO ()
bindFN stmt (FuncName fn) = void $ bindBlob stmt 1 fn

execStmt :: Database -> Utf8 -> (Statement -> IO ()) -> IO ()
execStmt db sql bindStmt = do
  stmt <- prepStmt db sql
  bindStmt stmt
  void $ liftEither $ step stmt
  void $ finalize stmt

execFN :: Database -> Utf8 -> FuncName -> IO ()
execFN db sql fn = execStmt db sql (`bindFN` fn)

queryStmt :: Database -> Utf8 -> (Statement -> IO ()) -> (Statement -> IO a) -> IO a
queryStmt db sql bindStmt stepStmt = do
  stmt <- prepStmt db sql
  bindStmt stmt
  ret <- stepStmt stmt
  void $ finalize stmt
  pure ret

bindFnAndJn :: Byteable k => FuncName -> k -> Statement -> IO ()
bindFnAndJn fn jn stmt = do
  bindFN stmt fn
  void $ bindBlob stmt 2 $ toBytes jn

mkFoldFunc :: (Job -> a -> a) -> ByteString -> a -> a
mkFoldFunc f bs acc =
  case runParser bs of
    Left _    -> acc
    Right job -> f job acc

foldStmt :: (ByteString -> a -> a) -> a -> Statement -> IO a
foldStmt f acc stmt = do
  sr <- liftEither $ step stmt
  case sr of
    Done -> pure acc
    Row -> do
      bs <- columnBlob stmt 0
      foldStmt f (f bs acc) stmt

doFoldr_ :: Database -> Utf8 -> (Statement -> IO ()) -> (ByteString -> a -> a) -> a -> IO a
doFoldr_ db sql bindStmt f acc = queryStmt db sql bindStmt $ foldStmt f acc

stepInt64 :: Statement -> IO Int64
stepInt64 stmt = do
  sr <- liftEither $ step stmt

  case sr of
    Done -> pure 0
    Row  -> columnInt64 stmt 0
