{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.ClientCommand
  (
    ClientCommand (..)
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString, append, drop, head)
import           Periodic.Types.Internal
import           Periodic.Types.Job      (Job)
import           Prelude                 hiding (drop, head)

data ClientCommand =
    SubmitJob Job
  | Status
  | Ping
  | DropFunc ByteString
  | RemoveJob Job
  | Dump
  | Load
  | Shutdown

  deriving (Show)

instance Byteable ClientCommand where
  toBytes (SubmitJob job) = "\13" `append` nullChar `append` toBytes job
  toBytes Status          = "\14"
  toBytes Ping            = "\9"
  toBytes (DropFunc func) = "\15" `append` nullChar `append` func
  toBytes (RemoveJob job) = "\17" `append` nullChar `append` toBytes job
  toBytes Dump            = "\18"
  toBytes Load            = "\19"
  toBytes Shutdown        = "\20"

instance Parser ClientCommand where
  runParser bs = case head bs of
                   13 -> do
                     job <- runParser $ drop 3 bs
                     return (SubmitJob job)
                   14 -> Right Status
                   09 -> Right Ping
                   15 -> Right (DropFunc $ drop 3 bs)
                   17 -> do
                     job <- runParser $ drop 3 bs
                     return (RemoveJob job)
                   18 -> Right Dump
                   19 -> Right Load
                   20 -> Right Shutdown
