{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.ClientCommand
  (
    ClientCommand (..)
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString, append, take)
import           Periodic.Types.Internal
import           Periodic.Types.Job      (Job)
import           Prelude                 hiding (take)

data ClientCommand =
    SubmitJob Job
  | Status
  | Ping
  | DropFunc ByteString
  | RemoveJob Job
  | Dump
  | Load ByteString
  | Shutdown

  deriving (Show)

instance Byteable ClientCommand where
  toBytes (SubmitJob job) = "\13" `append` nullChar `append` toBytes job
  toBytes Status          = "\14"
  toBytes Ping            = "\9"
  toBytes (DropFunc func) = "\15" `append` nullChar `append` func
  toBytes (RemoveJob job) = "\17" `append` nullChar `append` toBytes job
  toBytes Dump            = "\18"
  toBytes (Load bs)       = "\19" `append` nullChar `append` bs
  toBytes Shutdown        = "\20"

instance Parser ClientCommand where
  runParser bs = case take 1 bs of
                   "\13" -> do
                     job <- runParser $ dropCmd bs
                     return (SubmitJob job)
                   "\14" -> Right Status
                   "\09" -> Right Ping
                   "\15" -> Right (DropFunc $ dropCmd bs)
                   "\17" -> do
                     job <- runParser $ dropCmd bs
                     return (RemoveJob job)
                   "\18" -> Right Dump
                   "\19" -> Right (Load $ dropCmd bs)
                   "\20" -> Right Shutdown
                   _ -> Left "Error ClientCommand"
