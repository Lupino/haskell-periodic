{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Periodic.Types.Internal
  (
    Parser (..)
  , FromBS (..)
  , parseBinary
  , ConfigKey (..)
  , LockName (..)
  , Validatable (..)
  , validateLength
  , validateNum
  ) where

import           Data.Binary              (Binary (..), decodeOrFail)
import           Data.Binary.Get          (getByteString, getWord8)
import           Data.Binary.Put          (putByteString, putWord8)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Char8    as B (length, pack, unpack)
import qualified Data.ByteString.Lazy     as LB (ByteString, fromStrict)
import           Data.Hashable
import           Data.Int                 (Int32)
import           Data.String              (IsString (..))
import           Data.Text                (Text)
import qualified Data.Text                as T (unpack)
import           Data.Text.Encoding       (decodeUtf8With)
import           Data.Text.Encoding.Error (ignore)
import qualified Data.Text.Lazy           as LT (Text, fromStrict)
import           GHC.Generics             (Generic)

class Parser a where
  runParser :: ByteString -> Either String a


parseBinary :: Binary a => ByteString -> Either String a
parseBinary bs = case decodeOrFail (LB.fromStrict bs) of
                   Left (_, _, e)  -> Left e
                   Right (_, _, v) -> Right v

class FromBS a where
  fromBS :: ByteString -> a

instance FromBS Text where
  fromBS = decodeUtf8With ignore

instance FromBS [Char] where
  fromBS = T.unpack . fromBS

instance FromBS LT.Text where
  fromBS = LT.fromStrict . fromBS

instance FromBS LB.ByteString where
  fromBS = LB.fromStrict

instance FromBS ByteString where
  fromBS = id

newtype ConfigKey = ConfigKey String
  deriving (Show)

instance Binary ConfigKey where
  get = do
    size <- getWord8
    dat <- getByteString $ fromIntegral size
    return $ ConfigKey $ B.unpack dat
  put (ConfigKey dat) = do
    putWord8 . fromIntegral $ length dat
    putByteString $ B.pack dat

newtype LockName = LockName ByteString
  deriving (Generic, Eq, Ord, Show)

instance Hashable LockName

instance Binary LockName where
  get = do
    size <- getWord8
    dat <- getByteString $ fromIntegral size
    return $ LockName dat
  put (LockName dat) = do
    putWord8 . fromIntegral $ B.length dat
    putByteString dat

instance IsString LockName where
  fromString = LockName . fromString

class Validatable a where
  validate :: a -> Either String ()

instance (Validatable a) => Validatable [a] where
  validate [] = Right ()
  validate (x:xs) = do
    validate x
    validate xs

instance Validatable ByteString where
  validate bs = validateLength "Data" 0 maxBound $ B.length bs

validateLength :: String -> Int32 -> Int32 -> Int -> Either String ()
validateLength n min' max' l'
  | l < min' = Left $ n ++ " is to short"
  | l > max' = Left $ n ++ " is to long"
  | otherwise = Right ()
  where l = fromIntegral l'

validateNum :: (Ord a) => String -> a -> a -> a -> Either String ()
validateNum n min' max' l
  | l < min' = Left $ n ++ " is to small"
  | l > max' = Left $ n ++ " is to big"
  | otherwise = Right ()

instance Validatable LockName where
  validate (LockName bs) = validateLength "LockName" 1 255 $ B.length bs

instance Validatable ConfigKey where
  validate (ConfigKey k) = validateLength "ConfigKey" 1 255 $ length k
