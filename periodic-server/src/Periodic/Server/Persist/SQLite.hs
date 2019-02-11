{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Periodic.Server.Persist.SQLite
  ( initSQLite
  ) where

import           Prelude                 hiding (foldr, lookup)

import           Control.Monad           (void)
import           Control.Monad.Catch
import           Data.Byteable           (toBytes)
import           Data.ByteString         (ByteString)
import qualified Data.Foldable           as F (foldrM)
import           Data.Int                (Int64)
import           Data.Maybe              (isJust, listToMaybe)
import           Database.SQLite3.Direct
import           Periodic.Server.Persist
import           Periodic.Types.Internal (runParser)
import           Periodic.Types.Job      (FuncName (..), Job, JobName (..),
                                          getSchedAt)
import           System.Log.Logger       (errorM)


initSQLite :: Utf8 -> IO Persist
initSQLite path = do
  edb <- open path
  case edb of
    Left (e, _) -> dbError $ show e
    Right db -> do
      beginTx db
      createJobTable db
      createFuncTable db
      allPending db
      commitTx db
      pure $ Persist
        { member = doMember db
        , lookup = doLookup db
        , insert = doInsert db
        , delete = doDelete db
        , size = doSize db
        , foldr = doFoldr db
        , foldr' = doFoldr' db
        , insertFuncName = doInsertFuncName db
        , removeFuncName = doRemoveFuncName db
        , funcList = doFuncList db
        , minSchedAt = doMinSchedAt db Pending
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

createJobTable :: Database -> IO ()
createJobTable db = void . exec db $ Utf8 $
  "CREATE TABLE IF NOT EXISTS jobs ("
    <> "func CHAR(256) NOT NULL,"
    <> " name CHAR(256) NOT NULL,"
    <> " value BLOB,"
    <> " state  INTEGER DEFAULT 0,"
    <> " sched_at INTEGER DEFAULT 0,"
    <> " PRIMARY KEY (func, name))"

createFuncTable :: Database -> IO ()
createFuncTable db = void . exec db $ Utf8 $
  "CREATE TABLE IF NOT EXISTS funcs ("
    <> "func CHAR(256) NOT NULL,"
    <> " PRIMARY KEY (func))"

allPending :: Database -> IO ()
allPending db = void . exec db $ Utf8 "UPDATE jobs SET state=0"

doLookup :: Database -> State -> FuncName -> JobName -> IO (Maybe Job)
doLookup db state fn jn =
  listToMaybe <$> doFoldr_ db sql (bindFnAndJn fn jn) (mkFoldFunc f) []
  where sql = Utf8 $ "SELECT value FROM jobs WHERE func=? AND name=? AND state=" <> stateName state <> " LIMIT 1"
        f :: Job -> [Job] -> [Job]
        f job acc = job : acc

doMember :: Database -> State -> FuncName -> JobName -> IO Bool
doMember db st fn jn = isJust <$> doLookup db st fn jn

doInsert :: Database -> State -> FuncName -> JobName -> Job -> IO ()
doInsert db state fn jn job = do
  execStmt db sql $ \stmt -> do
    bindFnAndJn fn jn stmt
    void $ bindBlob  stmt 3 $ toBytes job
    void $ bindInt64 stmt 4 $ stateName' state
    void $ bindInt64 stmt 5 $ getSchedAt job
  doInsertFuncName db fn
  where sql = Utf8 "INSERT OR REPLACE INTO jobs VALUES (?, ?, ?, ?, ?)"

doInsertFuncName :: Database -> FuncName -> IO ()
doInsertFuncName db = execFN db sql
  where sql = Utf8 "INSERT OR REPLACE INTO funcs VALUES (?)"

doFoldr :: Database -> State -> (Job -> a -> a) -> a -> IO a
doFoldr db state f = doFoldr_ db sql (const $ pure ()) (mkFoldFunc f)
  where sql = Utf8 $ "SELECT value FROM jobs WHERE state=" <> stateName state

doFoldr' :: Database -> State -> [FuncName] -> (Job -> a -> a) -> a -> IO a
doFoldr' db state fns f acc = F.foldrM (foldFunc f) acc fns
  where sql = Utf8 $ "SELECT value FROM jobs WHERE func=? AND state=" <> stateName state

        foldFunc :: (Job -> a -> a) -> FuncName -> a -> IO a
        foldFunc  f0 fn =
          doFoldr_ db sql (`bindFN` fn) (mkFoldFunc f0)

doFuncList :: Database -> IO [FuncName]
doFuncList db =
  doFoldr_ db sql (const $ pure ()) (\fn acc -> FuncName fn : acc) []
  where sql = Utf8 "SELECT func FROM funcs"

doDelete :: Database -> FuncName -> JobName -> IO ()
doDelete db fn jn = execStmt db sql $ bindFnAndJn fn jn
  where sql = Utf8 "DELETE FROM jobs WHERE func=? AND name=?"

doRemoveFuncName :: Database -> FuncName -> IO ()
doRemoveFuncName db fn = do
  execFN db sql0 fn
  execFN db sql1 fn

  where sql0 = Utf8 "DELETE FROM funcs WHERE func=?"
        sql1 = Utf8 "DELETE FROM jobs WHERE func=?"

doMinSchedAt :: Database -> State -> FuncName -> IO Int64
doMinSchedAt db state fn = queryStmt db sql (`bindFN` fn) stepInt64
  where sql = Utf8 $ "SELECT sched_at FROM jobs WHERE func=? AND state=" <> stateName state <> " ORDER BY sched_at ASC LIMIT 1"

doSize :: Database -> State -> FuncName -> IO Int64
doSize db state fn = queryStmt db sql (`bindFN` fn) stepInt64
  where sql = Utf8 $ "SELECT COUNT(*) FROM jobs WHERE func=? AND state=" <> stateName state

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

bindFnAndJn :: FuncName -> JobName -> Statement -> IO ()
bindFnAndJn fn (JobName jn) stmt = do
  bindFN stmt fn
  void $ bindBlob stmt 2 jn

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
