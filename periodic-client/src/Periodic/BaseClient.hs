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

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (cons, empty, isInfixOf, unpack)
import           Periodic.Connection   (Connection, newClientConn, receive,
                                        send)

import qualified Periodic.Connection   as Conn (close)
import           Periodic.Socket       (Socket)
import           Periodic.Types        (ClientType, Error (..), Payload (..),
                                        noopError, nullChar)

import           Control.Exception     (bracket, try)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM (delete, elems, empty, insert,
                                              lookup)
import           Data.IORef            (IORef, atomicModifyIORef', newIORef)

import           Periodic.Agent        (Agent, agentID, feed)
import qualified Periodic.Agent        as Agent (newAgent)
import           System.Entropy        (getEntropy)

import           Periodic.Utils        (parsePayload)

import           Control.Concurrent    (ThreadId, forkIO, killThread)
import           Control.Monad         (forever, void, when)

import           Data.Maybe            (fromJust, isJust)

import           System.Log.Logger     (errorM, infoM)

data BaseClient = BaseClient { agents   :: IORef (HashMap ByteString Agent)
                             , conn     :: Connection
                             , threadID :: IORef (Maybe ThreadId)
                             }

newBaseClient :: Socket -> ClientType -> IO BaseClient
newBaseClient sock agentType = do
  conn <- newClientConn sock
  send conn $ (toEnum $ fromEnum agentType) `B.cons` B.empty
  void $ receive conn

  agents <- newIORef HM.empty
  threadID <- newIORef Nothing
  let bc = BaseClient {..}

  t <- forkIO $ forever $ mainLoop bc
  atomicModifyIORef' threadID (\_ -> (Just t, ()))
  infoM "Periodic.BaseClient" "Connected to periodic task system"
  return bc

addAgent :: BaseClient -> Agent -> IO ()
addAgent bc a = atomicModifyIORef' (agents bc) $ \v -> (HM.insert (agentID a) a v, ())

removeAgent :: BaseClient -> Agent -> IO ()
removeAgent bc a = atomicModifyIORef' (agents bc) $ \v -> (HM.delete (agentID a) v, ())

writePayload :: BaseClient -> Payload -> IO ()
writePayload bc pl@(Payload { payloadID = pid }) = do
  v <- atomicModifyIORef' (agents bc) $ \v -> (v, HM.lookup pid v)
  case v of
    Nothing    -> errorM "Periodic.BaseClient" $ "Agent [" ++ B.unpack pid ++ "] not found."
    Just agent -> feed agent pl

newAgent :: BaseClient -> IO Agent
newAgent bc = do
  aid <- getEntropy 4
  if B.isInfixOf nullChar aid then newAgent bc
                              else done aid

  where done :: ByteString -> IO Agent
        done aid = do
          agent <- Agent.newAgent (conn bc) aid
          addAgent bc agent
          return agent

withAgent :: BaseClient -> (Agent -> IO a) -> IO a
withAgent bc = bracket (newAgent bc) (removeAgent bc)

close :: BaseClient -> IO ()
close (BaseClient {..}) = do
  tid <- atomicModifyIORef' threadID (\v -> (v, v))
  when (isJust tid) $ killThread (fromJust tid)
  Conn.close conn

noopAgent :: BaseClient -> Error -> IO ()
noopAgent bc e = do
   v <- atomicModifyIORef' (agents bc) $ \v -> (v, HM.elems v)
   mapM_ (flip feed (noopError e)) v


mainLoop :: BaseClient -> IO ()
mainLoop bc = do
  e <- try $ receive (conn bc)
  case e of
    Left SocketClosed  -> noopAgent bc SocketClosed
    Left MagicNotMatch -> noopAgent bc MagicNotMatch
    Left _             -> return ()
    Right pl           -> writePayload bc (parsePayload pl)
