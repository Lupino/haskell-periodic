{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.Internal
  (
    nullChar
  , Parser (..)
  ) where

import           Data.ByteString (ByteString, append)

nullChar :: ByteString
nullChar = "\00\01"

class Parser a where
  runParser :: ByteString -> Either String a
