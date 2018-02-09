{-# LANGUAGE RecordWildCards #-}

module Periodic.Agent
  (
    Agent
  , newAgent
  , newEmptyAgent
  , send
  , send_
  , agentid
  , msgid
  , aAlive
  , feed
  , receive
  , receive_
  ) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as B (concat, cons, empty, null)

import           Periodic.Connection     (Connection, connected, connid)
import qualified Periodic.Connection     as Conn (send)

import           Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, takeMVar,
                                          tryPutMVar)
import           Control.Exception       (throwIO)
import           Control.Monad           (void)
import           Data.Byteable           (Byteable (..))
import           Periodic.Types.Internal
import           Periodic.Utils          (breakBS)

data Agent = Agent { aMsgid  :: ByteString
                   , aConn   :: Connection
                   , aReader :: MVar ByteString
                   }

newAgent :: ByteString -> Connection -> IO Agent
newAgent bs aConn = do
  let [aMsgid, pl] = breakBS 2 bs
  aReader <- newMVar pl
  return Agent {..}

newEmptyAgent :: ByteString -> Connection -> IO Agent
newEmptyAgent aMsgid aConn = do
  aReader <- newEmptyMVar
  return Agent {..}

agentid :: Agent -> ByteString
agentid Agent {..} = B.concat [ aMsgid, nullChar, connid aConn ]

msgid :: Agent -> ByteString
msgid = aMsgid

aAlive :: Agent -> IO Bool
aAlive Agent {..} = connected aConn

send_ :: Agent -> ByteString -> IO ()
send_ Agent{..} pl =
  Conn.send aConn $ B.concat [ aMsgid, nullChar, pl ]

send :: Byteable cmd => Agent -> cmd -> IO ()
send ag cmd = send_ ag $ toBytes cmd

feed :: Agent -> ByteString -> IO ()
feed Agent{..} = void . tryPutMVar aReader

receive :: Parser cmd => Agent -> IO (Either String cmd)
receive Agent{..} = runParser <$> takeMVar aReader

receive_ :: Agent -> IO ByteString
receive_ Agent{..} = takeMVar aReader
