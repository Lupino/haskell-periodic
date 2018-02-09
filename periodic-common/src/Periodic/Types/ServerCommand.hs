{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.ServerCommand
  (
    ServerCommand (..)
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString, append, drop, head)
import           Periodic.Types.Internal
import           Periodic.Types.Job      (Job)
import           Prelude                 hiding (drop, head)

data ServerCommand =
    Noop
  | JobAssign Job
  | NoJob
  | Pong
  | Unknown
  | Success

  deriving (Show)

instance Byteable ServerCommand where
  toBytes Noop            = "\00"
  toBytes (JobAssign job) = "\05" `append` nullChar `append` toBytes job
  toBytes NoJob           = "\06"
  toBytes Pong            = "\10"
  toBytes Unknown         = "\12"
  toBytes Success         = "\16"

instance Parser ServerCommand where
  runParser bs = case head bs of
                   00 -> Right Noop
                   05 -> do
                     job <- runParser (drop 3 bs)
                     return (JobAssign job)
                   06 -> Right NoJob
                   10 -> Right Pong
                   12 -> Right Unknown
                   16 -> Right Success
