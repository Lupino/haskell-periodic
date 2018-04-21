{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module Periodic.Types.Internal
  (
    Parser (..)
  , FromBS (..)
  , parseBinary
  , ConfigKey (..)
  ) where

import           Data.Binary              (Binary (..), decodeOrFail)
import           Data.Binary.Get          (getByteString, getWord32be, getWord8)
import           Data.Binary.Put          (putByteString, putWord32be, putWord8)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Char8    (pack, unpack)
import qualified Data.ByteString.Lazy     as LB (ByteString, fromStrict)
import           Data.Text                (Text)
import qualified Data.Text                as T (unpack)
import           Data.Text.Encoding       (decodeUtf8With)
import           Data.Text.Encoding.Error (ignore)
import qualified Data.Text.Lazy           as LT (Text, fromStrict)

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

data ConfigKey = ConfigKey String
 deriving (Show)
instance Binary ConfigKey where
  get = do
    size <- getWord8
    dat <- getByteString $ fromIntegral size
    return $ ConfigKey $ unpack dat
  put (ConfigKey dat) = do
    putWord8 . fromIntegral $ length dat
    putByteString $ pack dat
