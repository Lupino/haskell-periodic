{-# LANGUAGE OverloadedStrings #-}

module Periodic.Types.Payload
  (
    Payload (..)
  , payload
  , noopError
  , nullChar
  , nullCharLength
  ) where

import           Periodic.Types.Command (Command (Noop))
import           Periodic.Types.Error   (Error (EmptyError))

import           Data.ByteString        (ByteString, empty, length)
import           Prelude                hiding (length)

nullChar :: ByteString
nullChar = "\00\01"

nullCharLength :: Int
nullCharLength = length nullChar

data Payload = Payload { payloadID    :: ByteString
                       , payloadCMD   :: Command
                       , payloadData  :: ByteString
                       , payloadError :: Error
                       }
  deriving (Eq, Show)

payload :: ByteString -> Command -> Payload
payload pid cmd = Payload { payloadID    = pid
                          , payloadCMD   = cmd
                          , payloadData  = empty
                          , payloadError = EmptyError
                          }

noopError :: Error -> Payload
noopError e = (payload empty Noop) { payloadError = e }

