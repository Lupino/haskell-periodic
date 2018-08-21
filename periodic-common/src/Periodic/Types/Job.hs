{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Periodic.Types.Job
  (
    FuncName (..)
  , JobName (..)
  , Workload (..)
  , JobHandle
  , Job (..)
  , newJob
  , hashJobName
  , jHandle
  , unHandle
  , jobHandle
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B (empty, length)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Hashable
import           Data.Int                (Int64)
import           Data.Word               (Word64)
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

data JobHandle = JobHandle FuncName ByteString
  deriving (Generic, Eq, Ord, Show)

instance Hashable JobHandle

instance Byteable JobHandle where
  toBytes = toStrict . encode

instance Parser JobHandle where
  runParser = parseBinary

instance Binary JobHandle where
  get = do
    size <- getWord8
    fn <- getByteString $ fromIntegral (size - 8)
    jn <- getByteString 8
    return $ JobHandle (FuncName fn) jn
  put (JobHandle (FuncName fn) jn) = do
    putWord8 . fromIntegral $ B.length fn + 8
    putByteString fn
    putByteString jn

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


data JVer = V0 | V1

toVer :: Int -> JVer
toVer 0 = V0
toVer 1 = V1
toVer _ = V0

fromVer :: JVer -> Int
fromVer V0 = 0
fromVer V1 = 1

calcVer :: Job -> JVer
calcVer (Job {jCount = count})
  | count > 0 = V1
  | otherwise = V0


instance Binary Job where
  get = do
    jFuncName <- get
    jName <- get
    jWorkload <- get
    jSchedAt <- getInt64be
    ver <- toVer . fromIntegral <$> getWord8
    jCount <- case ver of
                V0 -> pure 0
                V1 -> fromIntegral <$> getInt32be
    return Job {..}
  put j@Job {..} = do
    put jFuncName
    put jName
    put jWorkload
    putInt64be jSchedAt
    let ver = calcVer j

    putWord8 $ fromIntegral $ fromVer ver

    case ver of
      V0 -> pure ()
      V1 -> putInt32be $ fromIntegral jCount

newJob :: FuncName -> JobName -> Job
newJob jFuncName jName = Job { jWorkload = Workload B.empty
                             , jSchedAt = 0
                             , jCount = 0
                             , ..
                             }

hashJobName :: JobName -> ByteString
hashJobName = toStrict . encode . toWord64 . hash
  where toWord64 :: Int -> Word64
        toWord64 = fromIntegral

jHandle :: Job -> JobHandle
jHandle Job{ jFuncName = fn
           , jName = jn
           } = jobHandle fn jn

unHandle :: JobHandle -> (FuncName, ByteString)
unHandle (JobHandle fn jn) = (fn, jn)

jobHandle :: FuncName -> JobName -> JobHandle
jobHandle fn jn = JobHandle fn $ hashJobName jn
