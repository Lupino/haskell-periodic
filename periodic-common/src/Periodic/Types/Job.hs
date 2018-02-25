{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Periodic.Types.Job
  (
    FuncName (..)
  , JobName (..)
  , Workload (..)
  , JobHandle (..)
  , Job (..)
  , newJob
  , hashJobName
  , jHandle
  , unHandle
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B (concat, drop, empty, head,
                                               length, take)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Hashable
import           Data.Int                (Int64)
import           GHC.Generics            (Generic)

import           Data.String             (IsString (..))
import           Periodic.Types.Internal

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

newtype FuncName  = FuncName {unFN :: ByteString}
  deriving (Generic, Eq, Ord, Show)

instance Hashable FuncName

instance Byteable FuncName where
  toBytes = toStrict . encode

instance Parser FuncName where
  runParser = parseBinary

instance IsString FuncName where
  fromString = FuncName . fromString

instance FromBS FuncName where
  fromBS = FuncName . fromBS

instance Binary FuncName where
  get = do
    size <- getWord8
    dat <- getByteString $ fromIntegral size
    return $ FuncName dat
  put (FuncName dat) = do
    putWord8 . fromIntegral $ B.length dat
    putByteString dat

newtype JobName   = JobName {unJN :: ByteString}
  deriving (Generic, Eq, Ord, Show)

instance Hashable JobName

instance Byteable JobName where
  toBytes = toStrict . encode

instance Parser JobName where
  runParser = parseBinary

instance IsString JobName where
  fromString = JobName . fromString

instance FromBS JobName where
  fromBS = JobName . fromBS

instance Binary JobName where
  get = do
    size <- getWord8
    dat <- getByteString $ fromIntegral size
    return $ JobName dat
  put (JobName dat) = do
    putWord8 . fromIntegral $ B.length dat
    putByteString dat

newtype JobHandle = JobHandle {unJH :: ByteString}
  deriving (Generic, Eq, Ord, Show)

instance Hashable JobHandle

instance Byteable JobHandle where
  toBytes = toStrict . encode

instance Parser JobHandle where
  runParser = parseBinary

instance IsString JobHandle where
  fromString = JobHandle . fromString

instance FromBS JobHandle where
  fromBS = JobHandle . fromBS

instance Binary JobHandle where
  get = do
    size <- getWord16be
    dat <- getByteString $ fromIntegral size
    return $ JobHandle dat
  put (JobHandle dat) = do
    putWord16be . fromIntegral $ B.length dat
    putByteString dat

newtype Workload  = Workload {unWL :: ByteString}
  deriving (Generic, Eq, Ord, Show)

instance Hashable Workload

instance Byteable Workload where
  toBytes = toStrict . encode

instance Parser Workload where
  runParser = parseBinary

instance IsString Workload where
  fromString = Workload . fromString

instance FromBS Workload where
  fromBS = Workload . fromBS

instance Binary Workload where
  get = do
    size <- getWord32be
    dat <- getByteString $ fromIntegral size
    return $ Workload dat
  put (Workload dat) = do
    putWord32be . fromIntegral $ B.length dat
    putByteString dat

data Job = Job { jFuncName :: FuncName
               , jName     :: JobName
               , jWorkload :: Workload
               , jSchedAt  :: Int64
               , jCount    :: Int
               }
  deriving (Show)

instance Byteable Job where
  toBytes = toStrict . encode

instance Parser Job where
  runParser = parseBinary

instance Binary Job where
  get = do
    jFuncName <- get
    jName <- get
    jWorkload <- get
    jSchedAt <- getInt64be
    jCount <- fromIntegral <$> getInt32be
    return Job {..}
  put Job {..} = do
    put jFuncName
    put jName
    put jWorkload
    putInt64be jSchedAt
    putInt32be $ fromIntegral jCount

newJob :: FuncName -> JobName -> Job
newJob jFuncName jName = Job { jWorkload = Workload B.empty
                             , jSchedAt = 0
                             , jCount = 0
                             , ..
                             }

hashJobName :: JobName -> ByteString
hashJobName = toStrict . encode . hash

jHandle :: Job -> JobHandle
jHandle Job{ jFuncName = FuncName fn
           , jName = jn
           } = JobHandle $ B.concat [ toStrict $ encode fn, hashJobName jn ]

unHandle :: JobHandle -> (FuncName, ByteString)
unHandle (JobHandle bs) = (fn, jn)
  where fnLen = fromEnum $ B.head bs
        fn = FuncName . B.take fnLen $ B.drop 1 bs
        jn = B.drop (fnLen + 1) bs
