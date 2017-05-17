{-# LANGUAGE OverloadedStrings #-}

module Periodic.Types
  (
    Command (..)
  , nullChar
  , ClientType (..)
  , Payload (..)
  , payload
  ) where

import           Data.ByteString (ByteString, empty)
import           GHC.Enum        (boundedEnumFrom, boundedEnumFromThen)

data Command = Noop -- server
             -- GrabJob client ask a job
             | GrabJob -- client
             -- SchedLater tell server sched later the job
             | SchedLater -- client
             -- WorkDone tell server the work is done
             | WorkDone -- client
             -- WorkFail tell server work is fail
             | WorkFail -- client
             -- JobAssign assign a job for client
             | JobAssign -- server
             -- NoJob tell client job is empty
             | NoJob -- server
             -- CanDo tell server the client can do some func
             | CanDo -- client
             -- CantDo tell server the client can not do some func
             | CantDo -- client
             -- Ping test ping
             | Ping -- client
             -- Pong reply pong
             | Pong -- server
             -- Sleep tell the client to sleep
             | Sleep -- client
             -- Unknown command unknow
             | Unknown -- server
             -- SubmitJob submit a job for server
             | SubmitJob -- client
             -- Status ask the server status
             | Status -- client
             -- DropFunc drop an empty worker func
             | DropFunc -- client
             -- Success reply client success
             | Success -- server
             -- RemoveJob remove a job
             | RemoveJob -- client
             -- Dump dump the data
             | Dump -- client
             -- Load load data to database
             | Load -- client

  deriving (Eq, Show)

instance Bounded Command where
  minBound = Noop
  maxBound = Load

instance Enum Command where
  succ Noop       = GrabJob
  succ GrabJob    = SchedLater
  succ SchedLater = WorkDone
  succ WorkDone   = WorkFail
  succ WorkFail   = JobAssign
  succ JobAssign  = NoJob
  succ NoJob      = CanDo
  succ CanDo      = CantDo
  succ CantDo     = Ping
  succ Ping       = Pong
  succ Pong       = Sleep
  succ Sleep      = Unknown
  succ Unknown    = SubmitJob
  succ SubmitJob  = Status
  succ Status     = DropFunc
  succ DropFunc   = Success
  succ Success    = RemoveJob
  succ RemoveJob  = Dump
  succ Dump       = Load
  succ Load       = errorWithoutStackTrace "Types.Command.succ: bad argument"

  pred Load       = Dump
  pred Dump       = RemoveJob
  pred RemoveJob  = Success
  pred Success    = DropFunc
  pred DropFunc   = Status
  pred Status     = SubmitJob
  pred SubmitJob  = Unknown
  pred Unknown    = Sleep
  pred Sleep      = Pong
  pred Pong       = Ping
  pred Ping       = CantDo
  pred CantDo     = CanDo
  pred CanDo      = NoJob
  pred NoJob      = JobAssign
  pred JobAssign  = WorkFail
  pred WorkFail   = WorkDone
  pred WorkDone   = SchedLater
  pred SchedLater = GrabJob
  pred GrabJob    = Noop
  pred Noop       = errorWithoutStackTrace "Types.Command.pred: bad argument"

  toEnum n | n == 0  = Noop
           | n == 1  = GrabJob
           | n == 2  = SchedLater
           | n == 3  = WorkDone
           | n == 4  = WorkFail
           | n == 5  = JobAssign
           | n == 6  = NoJob
           | n == 7  = CanDo
           | n == 8  = CantDo
           | n == 9  = Ping
           | n == 10 = Pong
           | n == 11 = Sleep
           | n == 12 = Unknown
           | n == 13 = SubmitJob
           | n == 14 = Status
           | n == 15 = DropFunc
           | n == 16 = Success
           | n == 17 = RemoveJob
           | n == 18 = Dump
           | n == 19 = Load
  toEnum _ = errorWithoutStackTrace "Types.Command.toEnum: bad argument"

  fromEnum Noop       = 0
  fromEnum GrabJob    = 1
  fromEnum SchedLater = 2
  fromEnum WorkDone   = 3
  fromEnum WorkFail   = 4
  fromEnum JobAssign  = 5
  fromEnum NoJob      = 6
  fromEnum CanDo      = 7
  fromEnum CantDo     = 8
  fromEnum Ping       = 9
  fromEnum Pong       = 10
  fromEnum Sleep      = 11
  fromEnum Unknown    = 12
  fromEnum SubmitJob  = 13
  fromEnum Status     = 14
  fromEnum DropFunc   = 15
  fromEnum Success    = 16
  fromEnum RemoveJob  = 17
  fromEnum Dump       = 18
  fromEnum Load       = 19

  -- Use defaults for the rest
  enumFrom     = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

nullChar :: ByteString
nullChar = "\00\01"

data ClientType = TypeClient | TypeWorker
  deriving (Eq, Show)

instance Bounded ClientType where
  minBound = TypeClient
  maxBound = TypeWorker

instance Enum ClientType where
  succ TypeClient = TypeWorker
  succ TypeWorker = errorWithoutStackTrace "Types.ClientType.succ: bad argument"

  pred TypeWorker = TypeClient
  pred TypeClient = errorWithoutStackTrace "Types.ClientType.pred: bad argument"

  toEnum n | n == 1  = TypeClient
           | n == 2  = TypeWorker
  toEnum _ = errorWithoutStackTrace "Types.ClientType.toEnum: bad argument"

  fromEnum TypeClient = 1
  fromEnum TypeWorker = 2

  -- Use defaults for the rest
  enumFrom     = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

data Payload = Payload { payloadID   :: ByteString
                       , payloadCMD  :: Command
                       , payloadData :: ByteString
                       }
  deriving (Eq, Show)

payload :: ByteString -> Command -> Payload
payload pid cmd = Payload { payloadID   = pid
                          , payloadCMD  = cmd
                          , payloadData = empty
                          }
