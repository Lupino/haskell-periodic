{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.ServerCommand
  (
    ServerCommand (..)
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString, append, concat, take)
import           Periodic.Types.Internal
import           Periodic.Types.Job      (Job)
import           Periodic.Utils          (breakBS2)
import           Prelude                 hiding (concat, take)

data ServerCommand =
    Noop
  | JobAssign ByteString Job
  | NoJob
  | Pong
  | Unknown
  | Success

  deriving (Show)

instance Byteable ServerCommand where
  toBytes Noop               = "\00"
  toBytes (JobAssign jh job) = concat ["\05", nullChar, jh, nullChar, toBytes job]
  toBytes NoJob              = "\06"
  toBytes Pong               = "\10"
  toBytes Unknown            = "\12"
  toBytes Success            = "\16"

instance Parser ServerCommand where
  runParser bs = case take 1 bs of
                   "\00" -> Right Noop
                   "\05" -> do
                        let (jh, pl) = breakBS2 $ dropCmd bs
                        job <- runParser pl
                        return (JobAssign jh job)
                   "\06" -> Right NoJob
                   "\10" -> Right Pong
                   "\12" -> Right Unknown
                   "\16" -> Right Success
                   _ -> Left "Error ServerCommand"
