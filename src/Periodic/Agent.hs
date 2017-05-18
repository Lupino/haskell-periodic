{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Agent
  (
    Agent
  , agentID
  , newAgent
  , feed
  , receive
  , send
  , send_
  ) where

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Data.ByteString.Char8   (ByteString)
import qualified Data.ByteString.Char8   as B (concat, cons, empty, null)
import           Periodic.Connection     (Connection)
import qualified Periodic.Connection     as Conn (send)
import           Periodic.Types          (Command, Payload, nullChar)

data Agent = Agent { _id     :: ByteString
                   , _conn   :: Connection
                   , _reader :: MVar Payload
                   }

agentID :: Agent -> ByteString
agentID agent = _id agent

newAgent :: Connection -> ByteString -> IO Agent
newAgent _conn _id = do
  _reader <- newEmptyMVar
  return Agent { .. }

feed :: Agent -> Payload -> IO ()
feed (Agent { _reader = r }) pl = putMVar r pl

receive :: Agent -> IO Payload
receive (Agent { _reader = r }) = takeMVar r

send_ :: Agent -> ByteString -> IO ()
send_ (Agent { _id = aid, _conn = conn }) pl =
  Conn.send conn $ B.concat [ aid, nullChar, pl ]

send :: Agent -> Command -> ByteString -> IO ()
send ag cmd pl =
  if B.null pl then send_ ag $ toB cmd
               else send_ ag $ B.concat [ toB cmd, nullChar, pl ]

  where toB x = (toEnum $ fromEnum x) `B.cons` B.empty
