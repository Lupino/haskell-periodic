{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.Agent
  (
    Agent
  , newAgent
  , send
  , send_
  , agentid
  , aAlive
  ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (concat, cons, empty, null)

import           Periodic.Connection   (Connection, connected, connid)
import qualified Periodic.Connection   as Conn (send)
import           Periodic.Types        (Command, nullChar)

data Agent = Agent { aMsgid :: ByteString
                   , aConn  :: Connection
                   }

agentid :: Agent -> ByteString
agentid (Agent {..}) = B.concat [ aMsgid, nullChar, connid aConn ]

aAlive :: Agent -> IO Bool
aAlive (Agent {..}) = connected aConn

newAgent :: ByteString -> Connection -> Agent
newAgent aMsgid aConn = Agent { .. }

send_ :: Agent -> ByteString -> IO ()
send_ (Agent { .. }) pl =
  Conn.send aConn $ B.concat [ aMsgid, nullChar, pl ]

send :: Agent -> Command -> ByteString -> IO ()
send ag cmd pl =
  if B.null pl then send_ ag $ toB cmd
               else send_ ag $ B.concat [ toB cmd, nullChar, pl ]

  where toB x = (toEnum $ fromEnum x) `B.cons` B.empty
