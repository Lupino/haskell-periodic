{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server.Persist.SQLite
  ( initSQLite
  ) where

import           Prelude                 hiding (foldr, lookup)

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Monad           (void)
import           Control.Monad.Catch
import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString, intercalate)
import qualified Data.Foldable           as F (foldrM)
import           Data.Int                (Int64)
import           Data.Maybe              (isJust)
import           Data.String             (fromString)
import           Database.SQLite3.Direct
import           Periodic.Server.Persist (Persist (..), Persister (..))
import           Periodic.Types.Internal (runParser)
import           Periodic.Types.Job      (FuncName (..), Job (jSchedAt))
import           System.Log.Logger       (errorM)

data Table = Table ByteString

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
      commitTx db
      pure $ Persist
        { main = Persister
          { member = doMember db mainTableName
          , lookup = doLookup db mainTableName
          , insert = doInsert db mainTableName
          , delete = doDelete db mainTableName
          , size = doSize db mainTableName
          , minSchedAt = doMinSchedAt db mainTableName
          , funcList = doFuncList db mainTableName
          , foldr = doFoldr db mainTableName
          , foldr' = doFoldr' db mainTableName
          }
        , proc = Persister
          { member = doMember db procTableName
          , lookup = doLookup db procTableName
          , insert = doInsert db procTableName
          , delete = doDelete db procTableName
          , size = doSize db procTableName
          , minSchedAt = doMinSchedAt db procTableName
          , funcList = doFuncList db procTableName
          , foldr = doFoldr db procTableName
          , foldr' = doFoldr' db procTableName
          }
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
doLookup db (Table tn) (FuncName fn) jn = do
  stmt <- prepStmt db sql
  bindBlob stmt 1 fn
  bindBlob stmt 2 $ toBytes jn
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
doInsert db (Table tn) (FuncName fn) jn job = do
  stmt <- prepStmt db sql
  bindBlob stmt 1 fn
  bindBlob stmt 2 $ toBytes jn
  bindBlob stmt 3 $ toBytes job
  bindInt64 stmt 4 $ jSchedAt job
  void $ liftEither $ step stmt
  void $ finalize stmt
  where sql = Utf8 $ "INSERT OR REPLACE INTO " <> tn <> " VALUES (?, ?, ?, ?)"

doFoldr_ :: Database -> Utf8 -> (Statement -> IO ()) -> (Job -> a -> a) -> a -> IO a
doFoldr_ db sql stepStmt f acc = do
  stmt <- prepStmt db sql

  stepStmt stmt

  ret <- foldStmt stmt (foldFunc f) acc

  void $ finalize stmt
  pure ret

  where foldFunc :: (Job -> a -> a) -> ByteString -> a -> a
        foldFunc f0 bs acc0 =
          case runParser bs of
            Left e    -> acc0
            Right job -> f0 job acc0

foldStmt :: Statement -> (ByteString -> a -> a) -> a -> IO a
foldStmt stmt f acc = do
  sr <- liftEither $ step stmt
  case sr of
    Done -> pure acc
    Row -> do
      bs <- columnBlob stmt 0
      foldStmt stmt f $ f bs acc

doFoldr :: Database -> Table -> (Job -> a -> a) -> a -> IO a
doFoldr db (Table tn) = doFoldr_ db sql (const $ pure ())
  where sql = Utf8 $ "SELECT value FROM " <> tn

doFoldr' :: Database -> Table -> [FuncName] -> (Job -> a -> a) -> a -> IO a
doFoldr' db (Table tn) fns f acc = F.foldrM (foldFunc f) acc fns
  where sql = Utf8 $ "SELECT value FROM " <> tn <> " WHERE func=?"

        foldFunc :: (Job -> a -> a) -> FuncName -> a -> IO a
        foldFunc  f0 (FuncName fn) acc0 =
          doFoldr_ db sql (\stmt -> void $ bindBlob stmt 1 fn) f0 acc0

doFuncList :: Database -> Table -> IO [FuncName]
doFuncList db (Table tn) = do
  stmt <- prepStmt db sql

  ret <- foldStmt stmt (\fn acc -> FuncName fn:acc) []

  void $ finalize stmt
  pure ret

  where sql = Utf8 $ "SELECT func FROM " <> tn <> " GROUP BY func"

doDelete :: Byteable k => Database -> Table -> FuncName -> k -> IO ()
doDelete db (Table tn) (FuncName fn) jn = do
  stmt <- prepStmt db sql
  bindBlob stmt 1 fn
  bindBlob stmt 2 $ toBytes jn
  void $ liftEither $ step stmt
  void $ finalize stmt
  where sql = Utf8 $ "DELETE FROM " <> tn <> " WHERE func=? and name=?"

doMinSchedAt :: Database -> Table -> FuncName -> IO Int64
doMinSchedAt db (Table tn) (FuncName fn) = do
  stmt <- prepStmt db sql
  bindBlob stmt 1 fn
  sr <- liftEither $ step stmt

  ret <- case sr of
    Done -> pure 0
    Row  -> columnInt64 stmt 0

  void $ finalize stmt
  pure ret

  where sql = Utf8 $ "SELECT sched_at FROM " <> tn <> " WHERE func=? ORDER BY sched_at ASC LIMIT 1"

doSize :: Database -> Table -> FuncName -> IO Int64
doSize db (Table tn) (FuncName fn) = do
  stmt <- prepStmt db sql
  bindBlob stmt 1 fn
  sr <- liftEither $ step stmt

  ret <- case sr of
    Done -> pure 0
    Row  -> columnInt64 stmt 0

  void $ finalize stmt
  pure ret

  where sql = Utf8 $ "SELECT COUNT(*) FROM " <> tn <> " WHERE func=?"
