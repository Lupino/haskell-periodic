{-# LANGUAGE OverloadedStrings #-}
module Periodic.Types.WorkerCommand
  (
    WorkerCommand (..)
  ) where

import           Data.Byteable           (Byteable (..))
import           Data.ByteString         (ByteString, concat, empty, take)
import           Data.ByteString.Char8   (pack)
import           Data.Int                (Int64)
import           Periodic.Types.Internal
import           Periodic.Types.Job      (FuncName, JobHandle)
import           Periodic.Utils          (breakBS, readBS)
import           Prelude                 hiding (concat, take)

data WorkerCommand =
    GrabJob
  | SchedLater JobHandle Int64 Int
  | WorkDone JobHandle
  | WorkFail JobHandle
  | Sleep
  | Ping
  | CanDo FuncName
  | CantDo FuncName
  | Broadcast FuncName

  deriving (Show)

instance Byteable WorkerCommand where
  toBytes GrabJob = "\01"
  toBytes (SchedLater jh later step) = concat $ ["\02", nullChar, toBytes jh, nullChar, pack $ show later ] ++ step'
    where step' | step > 0  = [nullChar, pack $ show step]
                | otherwise = []
  toBytes (WorkDone jh) = concat ["\03", nullChar, toBytes jh]
  toBytes (WorkFail jh) = concat ["\04", nullChar, toBytes jh]
  toBytes Sleep = "\11"
  toBytes Ping = "\9"
  toBytes (CanDo fn) = concat ["\07", nullChar, toBytes fn]
  toBytes (CantDo fn) = concat ["\08", nullChar, toBytes fn]
  toBytes (Broadcast fn) = concat ["\21", nullChar, toBytes fn]

instance Parser WorkerCommand where
  runParser bs = case take 1 bs of
                   "\01" -> Right GrabJob
                   "\02" -> do
                        let (bs', later, step) = parseSchedLater $ breakBS 3 (dropCmd bs)
                        jh <- runParser bs'
                        return (SchedLater jh later step)
                   "\03" -> do
                     jh <- runParser $ dropCmd bs
                     return (WorkDone jh)
                   "\04" -> do
                     jh <- runParser $ dropCmd bs
                     Right (WorkFail jh)
                   "\11" -> Right Sleep
                   "\09" -> Right Ping
                   "\07" -> do
                     fn <- runParser $ dropCmd bs
                     return (CanDo fn)
                   "\08" -> do
                     fn <- runParser $ dropCmd bs
                     return (CantDo fn)
                   "\21" -> do
                     fn <- runParser $ dropCmd bs
                     return (Broadcast fn)
                   _ -> Left "Error WorkerCommand"

parseSchedLater :: [ByteString] -> (ByteString, Int64, Int)
parseSchedLater (a:b:c:_) = (a, readBS b, readBS c)
parseSchedLater [a, b]    = (a, readBS b, 0)
parseSchedLater [a]       = (a, 0, 0)
parseSchedLater []        = (empty, 0, 0)
