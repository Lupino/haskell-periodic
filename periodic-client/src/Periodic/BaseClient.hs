{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.BaseClient
  (
    BaseClient
  , newBaseClient
  , removeAgent
  , newAgent
  , withAgent
  , noopAgent
  , close
  ) where

import           Data.ByteString     (ByteString)
import qualified Data.ByteString     as B (cons, empty, isInfixOf)
import           Periodic.Connection (Connection, newClientConn, receive, send)

import qualified Periodic.Connection as Conn (close)
import           Periodic.Transport  (Transport)
import           Periodic.Types      (ClientType, Error (..), Payload (..),
                                      noopError, nullChar)

import           Control.Exception   (bracket, try)
import           Periodic.IOHashMap  (IOHashMap, newIOHashMap)
import qualified Periodic.IOHashMap  as HM (delete, elems, insert, lookup)

import           Periodic.Agent      (Agent, feed, msgid)
import qualified Periodic.Agent      as Agent (newAgent')
import           System.Entropy      (getEntropy)

import           Periodic.Utils      (parsePayload)

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, void, when)
import           Periodic.TM

import           Data.Maybe          (fromJust, isJust)

import           System.Log.Logger   (errorM, infoM)

data BaseClient = BaseClient { agents :: IOHashMap Agent
                             , conn   :: Connection
                             , runner :: ThreadManager
                             }

newBaseClient :: Transport -> ClientType -> IO BaseClient
newBaseClient transport agentType = do
  conn <- newClientConn transport
  send conn $ (toEnum $ fromEnum agentType) `B.cons` B.empty
  void $ receive conn

  agents <- newIOHashMap
  runner <- newThreadManager
  let bc = BaseClient {..}

  t <- forkIO $ forever $ mainLoop bc
  setThreadId runner t
  infoM "Periodic.BaseClient" "Connected to periodic task system"
  return bc

addAgent :: BaseClient -> Agent -> IO ()
addAgent bc a = HM.insert (agents bc) (msgid a) a

removeAgent :: BaseClient -> Agent -> IO ()
removeAgent bc a = HM.delete (agents bc) (msgid a)

writePayload :: BaseClient -> Payload -> IO ()
writePayload bc pl@(Payload { payloadID = pid }) = do
  v <- HM.lookup (agents bc) pid
  case v of
    Nothing    -> errorM "Periodic.BaseClient" $ "Agent [" ++ show pid ++ "] not found."
    Just agent -> feed agent pl

newAgent :: BaseClient -> IO Agent
newAgent bc = do
  aid <- getEntropy 4
  if B.isInfixOf nullChar aid then newAgent bc
                              else done aid

  where done :: ByteString -> IO Agent
        done aid = do
          agent <- Agent.newAgent' aid $ conn bc
          addAgent bc agent
          return agent

withAgent :: BaseClient -> (Agent -> IO a) -> IO a
withAgent bc = bracket (newAgent bc) (removeAgent bc)

close :: BaseClient -> IO ()
close (BaseClient {..}) = do
  killThread runner
  Conn.close conn

noopAgent :: BaseClient -> Error -> IO ()
noopAgent bc e =
   mapM_ (flip feed (noopError e)) =<< HM.elems (agents bc)


mainLoop :: BaseClient -> IO ()
mainLoop bc = do
  e <- try $ receive (conn bc)
  case e of
    Left TransportClosed -> noopAgent bc TransportClosed
    Left MagicNotMatch   -> noopAgent bc MagicNotMatch
    Left _               -> return ()
    Right pl             -> writePayload bc (parsePayload pl)
