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
import           Data.Maybe              (isJust)
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

doLookup :: Byteable k => Database -> Table -> FuncName -> k -> IO (Maybe Job)
doLookup db (Table tn) fn jn = do
  stmt <- prepStmt db sql
  bindFnAndJn stmt fn jn
  sr <- liftEither $ step stmt
  ret <- case sr of
    Done -> pure Nothing
    Row -> do
      bs <- columnBlob stmt 0
      case runParser bs of
        Left e    -> dbError e
        Right job -> pure $ Just job

  void $ finalize stmt

  pure ret

  where sql = Utf8 $ "SELECT value FROM " <> tn <> " WHERE func=? AND name=? LIMIT 1"

doMember :: Byteable k => Database -> Table -> FuncName -> k -> IO Bool
doMember db tb fn jn = isJust <$> doLookup db tb fn jn

doInsert :: Byteable k => Database -> Table -> FuncName -> k -> Job -> IO ()
doInsert db (Table tn) fn jn job = do
  execStmt db sql $ \stmt -> do
    bindFnAndJn stmt fn jn
    void $ bindBlob stmt 3 $ toBytes job
    void $ bindInt64 stmt 4 $ jSchedAt job
  doInsertFuncName db fn
  where sql = Utf8 $ "INSERT OR REPLACE INTO " <> tn <> " VALUES (?, ?, ?, ?)"

doInsertFuncName :: Database -> FuncName -> IO ()
doInsertFuncName db = execFN db sql
  where sql = Utf8 "INSERT OR REPLACE INTO funcs VALUES (?)"

execStmt :: Database -> Utf8 -> (Statement -> IO ()) -> IO ()
execStmt db sql bindStmt = do
  stmt <- prepStmt db sql
  bindStmt stmt
  void $ liftEither $ step stmt
  void $ finalize stmt

execFN :: Database -> Utf8 -> FuncName -> IO ()
execFN db sql (FuncName fn) = execStmt db sql $ \stmt -> void $ bindBlob stmt 1 fn

bindFnAndJn :: Byteable k => Statement -> FuncName -> k -> IO ()
bindFnAndJn stmt (FuncName fn) jn = do
  void $ bindBlob stmt 1 fn
  void $ bindBlob stmt 2 $ toBytes jn

doFoldr_ :: Database -> Utf8 -> (Statement -> IO ()) -> (ByteString -> a -> a) -> a -> IO a
doFoldr_ db sql bindStmt f acc = do
  stmt <- prepStmt db sql

  bindStmt stmt

  ret <- foldStmt stmt f acc

  void $ finalize stmt
  pure ret

mkFoldFunc :: (Job -> a -> a) -> ByteString -> a -> a
mkFoldFunc f bs acc =
  case runParser bs of
    Left _    -> acc
    Right job -> f job acc

foldStmt :: Statement -> (ByteString -> a -> a) -> a -> IO a
foldStmt stmt f acc = do
  sr <- liftEither $ step stmt
  case sr of
    Done -> pure acc
    Row -> do
      bs <- columnBlob stmt 0
      foldStmt stmt f $ f bs acc

doFoldr :: Database -> Table -> (Job -> a -> a) -> a -> IO a
doFoldr db (Table tn) f = doFoldr_ db sql (const $ pure ()) (mkFoldFunc f)
  where sql = Utf8 $ "SELECT value FROM " <> tn

doFoldr' :: Database -> Table -> [FuncName] -> (Job -> a -> a) -> a -> IO a
doFoldr' db (Table tn) fns f acc = F.foldrM (foldFunc f) acc fns
  where sql = Utf8 $ "SELECT value FROM " <> tn <> " WHERE func=?"

        foldFunc :: (Job -> a -> a) -> FuncName -> a -> IO a
        foldFunc  f0 (FuncName fn) =
          doFoldr_ db sql (\stmt -> void $ bindBlob stmt 1 fn) (mkFoldFunc f0)

doFuncList :: Database -> IO [FuncName]
doFuncList db =
  doFoldr_ db sql (const $ pure ()) (\fn acc -> FuncName fn : acc) []
  where sql = Utf8 "SELECT func FROM funcs"

doDelete :: Byteable k => Database -> Table -> FuncName -> k -> IO ()
doDelete db (Table tn) (FuncName fn) jn = do
  stmt <- prepStmt db sql
  void $ bindBlob stmt 1 fn
  void $ bindBlob stmt 2 $ toBytes jn
  void $ liftEither $ step stmt
  void $ finalize stmt
  where sql = Utf8 $ "DELETE FROM " <> tn <> " WHERE func=? and name=?"

doRemoveFuncName :: Database -> FuncName -> IO ()
doRemoveFuncName db fn = do
  execFN db sql0 fn
  execFN db sql1 fn

  where sql0 = Utf8 "DELETE FROM funcs WHERE func=?"
        sql1 = Utf8 "DELETE FROM main WHERE func=?"

doMinSchedAt :: Database -> Table -> FuncName -> IO Int64
doMinSchedAt db (Table tn) = queryInt64 db sql
  where sql = Utf8 $ "SELECT sched_at FROM " <> tn <> " WHERE func=? ORDER BY sched_at ASC LIMIT 1"

doSize :: Database -> Table -> FuncName -> IO Int64
doSize db (Table tn) = queryInt64 db sql
  where sql = Utf8 $ "SELECT COUNT(*) FROM " <> tn <> " WHERE func=?"

queryInt64 :: Database -> Utf8 -> FuncName -> IO Int64
queryInt64 db sql (FuncName fn) = do
  stmt <- prepStmt db sql
  void $ bindBlob stmt 1 fn
  sr <- liftEither $ step stmt

  ret <- case sr of
    Done -> pure 0
    Row  -> columnInt64 stmt 0

  void $ finalize stmt
  pure ret
