{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.Internal
  (
    nullChar
  , nullCharLength
  , dropCmd
  , Parser (..)
  , FromBS (..)
  ) where

import           Data.Byteable
import           Data.ByteString          (ByteString, drop)
import qualified Data.ByteString.Lazy     as LB (ByteString, fromStrict)
import           Data.Text                (Text)
import qualified Data.Text                as T (unpack)
import           Data.Text.Encoding       (decodeUtf8With)
import           Data.Text.Encoding.Error (ignore)
import qualified Data.Text.Lazy           as LT (Text, fromStrict)
import           Prelude                  hiding (drop)

nullChar :: ByteString
nullChar = "\00\01"

nullCharLength :: Int
nullCharLength = 2

dropCmd :: ByteString -> ByteString
dropCmd = drop (nullCharLength + 1)

class Parser a where
  runParser :: ByteString -> Either String a

class FromBS a where
  fromBS :: Byteable b => b -> a

instance FromBS Text where
  fromBS = decodeUtf8With ignore . toBytes

instance FromBS [Char] where
  fromBS = T.unpack . fromBS

instance FromBS LT.Text where
  fromBS = LT.fromStrict . fromBS

instance FromBS LB.ByteString where
  fromBS = LB.fromStrict . toBytes

instance FromBS ByteString where
  fromBS = toBytes
