{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Periodic.Types.Job
  ( FuncName (..)
  , JobName (..)
  , Workload (..)
  , JobHandle
  , Job
  , initJob
  , setWorkload
  , setSchedAt
  , setCount
  , setTimeout
  , getFuncName
  , getName
  , getWorkload
  , getSchedAt
  , getCount
  , getTimeout
  , getHandle
  , unHandle
  , jobHandle
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B (empty, length)
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

instance Validatable FuncName where
  validate (FuncName n) = validateLength "FuncName" 1 255 $ B.length n

newtype JobName   = JobName {unJN :: ByteString}
  deriving (Generic, Eq, Ord, Show)

instance Hashable JobName

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

instance Validatable JobName where
  validate (JobName n) = validateLength "JobName" 1 255 $ B.length n

data JobHandle = JobHandle FuncName JobName
    deriving (Generic, Eq, Ord, Show)

instance Hashable JobHandle

instance Binary JobHandle where
  get = do
    fn <- get
    JobHandle fn <$> get
  put (JobHandle fn jn) = do
    put fn
    put jn

instance Validatable JobHandle where
  validate (JobHandle fn jn) = do
    validate fn
    validate jn

newtype Workload  = Workload {unWL :: ByteString}
  deriving (Generic, Eq, Ord, Show)

instance Hashable Workload

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

instance Validatable Workload where
  validate (Workload n) = validateLength "Workload" 0 maxBound $ B.length n

data Job = Job
    { jFuncName :: FuncName
    , jName     :: JobName
    , jWorkload :: Workload
    , jSchedAt  :: Int64
    , jCount    :: Int
    , jTimeout  :: Int
    }
    deriving (Show)

instance Byteable Job where
  toBytes = toStrict . encode

data JVer = V0
    | V1
    | V2
    | V3

toVer :: Int -> JVer
toVer 0 = V0
toVer 1 = V1
toVer 2 = V2
toVer 3 = V3
toVer _ = V0

fromVer :: JVer -> Int
fromVer V0 = 0
fromVer V1 = 1
fromVer V2 = 2
fromVer V3 = 3

calcVer :: Job -> JVer
calcVer Job{jCount = count, jTimeout = to}
  | count > 0 && to > 0 = V3
  | to > 0 = V2
  | count > 0 = V1
  | otherwise = V0


instance Binary Job where
  get = do
    jFuncName <- get
    jName <- get
    jWorkload <- get
    jSchedAt <- getInt64be
    ver <- toVer . fromIntegral <$> getWord8
    (jCount, jTimeout) <-
      case ver of
        V0 -> pure (0, 0)
        V1 -> do
          v <- fromIntegral <$> getInt32be
          pure (v, 0)
        V2 -> do
          v <- fromIntegral <$> getInt32be
          pure (0, v)
        V3 -> do
          v0 <- fromIntegral <$> getInt32be
          v1 <- fromIntegral <$> getInt32be
          pure (v0, v1)
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
      V2 -> putInt32be $ fromIntegral jTimeout
      V3 -> do
        putInt32be $ fromIntegral jCount
        putInt32be $ fromIntegral jTimeout

instance Validatable Job where
  validate (Job fn jn w _ c t) = do
    validate fn
    validate jn
    validate w
    validateNum "JobCount" 0 maxBound c
    validateNum "JobTimeout" 0 maxBound t

initJob :: FuncName -> JobName -> Job
initJob jFuncName jName = Job
  { jWorkload = Workload B.empty
  , jSchedAt = 0
  , jCount = 0
  , jTimeout = 0
  , ..
  }

setSchedAt :: Int64 -> Job -> Job
setSchedAt schedAt job = job {jSchedAt = schedAt}

setWorkload :: Workload -> Job -> Job
setWorkload w job = job {jWorkload = w}

setCount :: Int -> Job -> Job
setCount c job = job {jCount = c}

setTimeout :: Int -> Job -> Job
setTimeout t job = job {jTimeout = t}

getFuncName :: Job -> FuncName
getFuncName = jFuncName

getName :: Job -> JobName
getName = jName

getSchedAt :: Job -> Int64
getSchedAt = jSchedAt

getWorkload :: Job -> Workload
getWorkload = jWorkload

getCount :: Job -> Int
getCount = jCount

getTimeout :: Job -> Int
getTimeout = jTimeout

getHandle :: Job -> JobHandle
getHandle job = jobHandle (getFuncName job) (getName job)

unHandle :: JobHandle -> (FuncName, JobName)
unHandle (JobHandle fn jn) = (fn, jn)

jobHandle :: FuncName -> JobName -> JobHandle
jobHandle = JobHandle
