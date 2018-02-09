{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.Internal
  (
    nullChar
  , nullCharLength
  , dropCmd
  , Parser (..)
  ) where

import           Data.ByteString (ByteString, drop)
import           Prelude         hiding (drop)

nullChar :: ByteString
nullChar = "\00\01"

nullCharLength :: Int
nullCharLength = 2

dropCmd :: ByteString -> ByteString
dropCmd = drop (nullCharLength + 1)

class Parser a where
  runParser :: ByteString -> Either String a
