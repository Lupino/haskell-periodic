{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.WorkerCommand
  (
    WorkerCommand (..)
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString, append, concat, empty,
                                          head)
import           Data.ByteString.Char8   (pack)
import           Data.Int                (Int64)
import           Periodic.Types.Internal
import           Periodic.Types.Job      (Job)
import           Periodic.Utils          (breakBS, readBS)
import           Prelude                 hiding (concat, head)

data WorkerCommand =
    GrabJob
  | SchedLater ByteString Int64 Int
  | WorkDone ByteString
  | WorkFail ByteString
  | Sleep
  | Ping
  | CanDo ByteString
  | CantDo ByteString
  | Broadcast ByteString

  deriving (Show)

instance Byteable WorkerCommand where
  toBytes GrabJob = "\01"
  toBytes (SchedLater bs later step) = concat $ ["\02", nullChar, bs, nullChar, pack $ show later ] ++ step'
    where step' | step > 0  = [nullChar, pack $ show step]
                | otherwise = []
  toBytes (WorkDone bs) = concat ["\03", nullChar, bs]
  toBytes (WorkFail bs) = concat ["\04", nullChar, bs]
  toBytes Sleep = "\11"
  toBytes Ping = "\9"
  toBytes (CanDo bs) = concat ["\07", nullChar, bs]
  toBytes (CantDo bs) = concat ["\08", nullChar, bs]
  toBytes (Broadcast bs) = concat ["\21", nullChar, bs]

instance Parser WorkerCommand where
  runParser bs = case head bs of
                   01 -> Right GrabJob
                   02 -> do
                     let (bs, later, step) = parseSchedLater $ breakBS 3 (dropCmd bs)
                     return (SchedLater bs later step)
                   03 -> Right (WorkDone $ dropCmd bs)
                   04 -> Right (WorkFail $ dropCmd bs)
                   11 -> Right Sleep
                   09 -> Right Ping
                   07 -> Right (CanDo $ dropCmd bs)
                   08 -> Right (CantDo $ dropCmd bs)
                   21 -> Right (Broadcast $ dropCmd bs)

parseSchedLater :: [ByteString] -> (ByteString, Int64, Int)
parseSchedLater (a:b:c:_) = (a, readBS b, readBS c)
parseSchedLater [a, b]    = (a, readBS b, 0)
parseSchedLater [a]       = (a, 0, 0)
parseSchedLater []        = (empty, 0, 0)
