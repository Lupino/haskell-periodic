{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Periodic.Server.Store
  (
    Store
  , newStore
  , lookupJob
  , deleteJob
  , insertJob
  , dumpJobHandle
  , dumpJob
  , existsJob
  , closeStore
  , archive
  ) where

import           Data.Acid            (AcidState, Query, Update, createArchive,
                                       makeAcidic, openLocalStateFrom, query,
                                       update)
import           Data.Acid.Local      (createCheckpointAndClose)


import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)
import           Data.SafeCopy        (base, deriveSafeCopy)

import           Data.Typeable        (Typeable)

import qualified Data.Map             as Map

import           Data.ByteString      (ByteString)
import           Data.Maybe           (catMaybes)
import           Periodic.Types       (Job, JobHandle, decodeJob, encodeJob,
                                       jHandle)
import           Prelude              hiding (lookup)

------------------------------------------------------
-- The Haskell structure that we want to encapsulate

type Key = ByteString
type Value = ByteString

data KeyValue = KeyValue !(Map.Map ByteString ByteString)
    deriving (Typeable)

$(deriveSafeCopy 0 'base ''KeyValue)

type Store = AcidState KeyValue

------------------------------------------------------
-- The transaction we will execute over the state.

insert :: Key -> Value -> Update KeyValue ()
insert key value
    = do KeyValue m <- get
         put (KeyValue (Map.insert key value m))

lookup :: Key -> Query KeyValue (Maybe Value)
lookup key
    = do KeyValue m <- ask
         return (Map.lookup key m)

delete :: Key -> Update KeyValue ()
delete key = do
  KeyValue m <- get
  put (KeyValue (Map.delete key m))

elems :: Query KeyValue [Value]
elems = do
  KeyValue m <- ask
  return (Map.elems m)

keys :: Query KeyValue [Key]
keys = do
  KeyValue m <- ask
  return (Map.keys m)

member :: Key -> Query KeyValue Bool
member key = do
  KeyValue m <- ask
  return (Map.member key m)

$(makeAcidic ''KeyValue ['insert, 'lookup, 'delete, 'elems, 'keys, 'member])

lookupJob :: Store -> JobHandle -> IO (Maybe Job)
lookupJob st jh = do
  rt <- query st (Lookup jh)
  case rt of
    Nothing -> return Nothing
    Just bs -> return $ decodeJob bs

insertJob :: Store -> Job -> IO ()
insertJob st job = update st (Insert (jHandle job) (encodeJob job))

deleteJob :: Store -> JobHandle -> IO ()
deleteJob st jh = update st (Delete jh)

dumpJob :: Store -> IO [Job]
dumpJob st = catMaybes . map decodeJob <$> query st Elems

dumpJobHandle :: Store -> IO [JobHandle]
dumpJobHandle st = query st Keys

existsJob :: Store -> JobHandle -> IO Bool
existsJob st jh = query st (Member jh)

newStore :: FilePath -> IO Store
newStore fp = openLocalStateFrom fp (KeyValue Map.empty)

closeStore :: Store -> IO ()
closeStore = createCheckpointAndClose

archive :: Store -> IO ()
archive = createArchive
