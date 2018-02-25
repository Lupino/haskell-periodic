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
  , decodeJob
  , encodeJob
  , hashJobName
  , jHandle
  , unHandle
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B (breakSubstring, concat, drop,
                                               empty, length, null, pack)
import           Data.Hashable
import           Data.Int                (Int64)
import           GHC.Generics            (Generic)

import           Data.Maybe              (catMaybes)
import           Data.String             (IsString (..))
import           Periodic.Types.Internal
import           Periodic.Utils          (breakBS, readBS)

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

newtype FuncName  = FuncName {unFN :: ByteString}
  deriving (Generic, Eq, Ord, Show)

instance Hashable FuncName

instance Byteable FuncName where
  toBytes = unFN

instance Parser FuncName where
  runParser bs = Right $ FuncName bs

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
  toBytes = unJN

instance Parser JobName where
  runParser bs = Right $ JobName bs

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
  toBytes = unJH

instance Parser JobHandle where
  runParser bs = Right $ JobHandle bs

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
  toBytes = unWL

instance Parser Workload where
  runParser bs = Right $ Workload bs

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
  toBytes = encodeJob

instance Parser Job where
  runParser bs = case decodeJob bs of
                   Nothing  -> Left "InvalidJob"
                   Just job -> Right job

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

encodeJob :: Job -> ByteString
encodeJob Job {..} = concatBS [ Just fn
                              , Just jn
                              , schedAt
                              , counter
                              , workload
                              ]

 where join :: [ByteString] -> [ByteString]
       join []     = []
       join [x]    = [x]
       join (x:xs) = x:nullChar:join xs

       concatBS = B.concat . join . catMaybes

       sched = B.pack $ show jSchedAt
       count = B.pack $ show jCount
       fn = unFN jFuncName
       jn = unJN jName
       jwl = unWL jWorkload

       schedAt | not (B.null jwl) = Just sched
               | jCount > 0       = Just sched
               | jSchedAt > 0     = Just sched
               | otherwise        = Nothing
       counter | not (B.null jwl) = Just count
               | jCount > 0       = Just count
               | otherwise        = Nothing

       workload | B.null jwl      = Nothing
                | otherwise       = Just jwl

decodeJob :: ByteString -> Maybe Job
decodeJob = go . breakBS 5
  where go :: [ByteString] -> Maybe Job
        go []            = Nothing
        go [_]           = Nothing
        go [x, y]        = Just $ newJob (FuncName x) (JobName y)
        go [x, y, z]     = Just $ (newJob (FuncName x) (JobName y))
          { jSchedAt = readBS z }
        go [x, y, z, a]  = Just $ (newJob (FuncName x) (JobName y))
          { jSchedAt = readBS z
          , jCount   = readBS a
          }
        go (x:y:z:a:b:_) = Just $ (newJob (FuncName x) (JobName y))
          { jWorkload = Workload b
          , jSchedAt  = readBS z
          , jCount    = readBS a
          }

sep :: ByteString
sep = "::"

sepLength :: Int
sepLength = B.length sep

hashJobName :: JobName -> ByteString
hashJobName = B.pack . show . hash

jHandle :: Job -> JobHandle
jHandle Job{ jFuncName = FuncName fn
           , jName = jn
           } = JobHandle $ B.concat [ fn, sep, hashJobName jn ]

unHandle :: JobHandle -> (FuncName, ByteString)
unHandle (JobHandle bs) = go $ B.breakSubstring sep bs
  where go (fn, jn) = (FuncName fn, B.drop sepLength jn)
