{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Agent
  (
    Agent
  , newAgent
  , newAgent'
  , send
  , send_
  , agentid
  , msgid
  , aAlive
  , feed
  , receive
  ) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B (concat, cons, empty, null)

import           Periodic.Connection     (Connection, connected, connid)
import qualified Periodic.Connection     as Conn (send)

import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception       (throwIO)
import           Data.Maybe              (fromJust)
import           Periodic.Types          (Command, Error (EmptyError),
                                          Payload (payloadError), nullChar)

data Agent = Agent { aMsgid  :: ByteString
                   , aConn   :: Connection
                   , aReader :: Maybe (MVar Payload)
                   }

newAgent :: ByteString -> Connection -> Agent
newAgent aMsgid aConn = Agent { aReader = Nothing, .. }

newAgent' :: ByteString -> Connection -> IO Agent
newAgent' aMsgid aConn = do
  reader <- newEmptyMVar
  return Agent { aReader = Just reader, .. }

agentid :: Agent -> ByteString
agentid (Agent {..}) = B.concat [ aMsgid, nullChar, connid aConn ]

msgid :: Agent -> ByteString
msgid = aMsgid

aAlive :: Agent -> IO Bool
aAlive (Agent {..}) = connected aConn

send_ :: Agent -> ByteString -> IO ()
send_ (Agent { .. }) pl =
  Conn.send aConn $ B.concat [ aMsgid, nullChar, pl ]

send :: Agent -> Command -> ByteString -> IO ()
send ag cmd pl =
  if B.null pl then send_ ag $ toB cmd
               else send_ ag $ B.concat [ toB cmd, nullChar, pl ]

  where toB x = (toEnum $ fromEnum x) `B.cons` B.empty

feed :: Agent -> Payload -> IO ()
feed (Agent {..}) pl = flip putMVar pl (fromJust aReader)

receive :: Agent -> IO Payload
receive (Agent {..}) = do
  pl <- takeMVar $ fromJust aReader
  if payloadError pl == EmptyError then return pl
                                   else throwIO $ payloadError pl
