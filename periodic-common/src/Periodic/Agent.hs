{-# LANGUAGE RecordWildCards #-}

module Periodic.Agent
  (
    Agent
  , AgentList
  , newAgent
  , newEmptyAgent
  , send
  , send_
  , agentid
  , msgid
  , msgidLength
  , aAlive
  , feed
  , receive
  , receive_
  , readerSize
  ) where

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B (concat, drop, take)

import           Periodic.Connection         (Connection, connected, connid)
import qualified Periodic.Connection         as Conn (send)

import           Control.Concurrent.STM.TVar
import           Control.Monad               (when)
import           Control.Monad.STM           (atomically, retry)
import           Data.Byteable               (Byteable (..))
import           Periodic.IOHashMap          (IOHashMap)
import           Periodic.Types.Internal

data Agent = Agent { aMsgid  :: ByteString
                   , aConn   :: Connection
                   , aReader :: TVar [ByteString]
                   }

type AgentList = IOHashMap ByteString Agent

newAgent :: ByteString -> Connection -> IO Agent
newAgent bs aConn = do
  aReader <- newTVarIO [B.drop msgidLength bs]
  return Agent {aMsgid = B.take msgidLength bs, ..}

newEmptyAgent :: ByteString -> Connection -> IO Agent
newEmptyAgent aMsgid aConn = do
  aReader <- newTVarIO []
  return Agent {..}

agentid :: Agent -> ByteString
agentid Agent {..} = B.concat [ aMsgid, connid aConn ]

msgid :: Agent -> ByteString
msgid = aMsgid

msgidLength :: Int
msgidLength = 4

aAlive :: Agent -> IO Bool
aAlive Agent {..} = connected aConn

send_ :: Agent -> ByteString -> IO ()
send_ Agent{..} pl =
  Conn.send aConn $ B.concat [ aMsgid, pl ]

send :: Byteable cmd => Agent -> cmd -> IO ()
send ag cmd = send_ ag $ toBytes cmd

feed :: Agent -> ByteString -> IO ()
feed Agent{..} dat = atomically . modifyTVar' aReader $ \v -> v ++ [dat]

receive :: Parser cmd => Agent -> IO (Either String cmd)
receive agent = runParser <$> receive_ agent

receive_ :: Agent -> IO ByteString
receive_ Agent{..} = atomically $ do
  v <- readTVar aReader
  when (null v) retry
  writeTVar aReader $ tail v
  pure $ head v

readerSize :: Agent -> IO Int
readerSize Agent{..} = length <$> readTVarIO aReader
