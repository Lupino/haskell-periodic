{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

#include "HsNetworkConfig.h"

#ifdef HAVE_GETADDRINFO
-- Use IPv6-capable function definitions if the OS supports it.
#define IPV6_SOCKET_SUPPORT 1
#endif

module Periodic.BaseClient
  (
    BaseClient
  , newBaseClient
  , removeAgent
  , newAgent
  , withAgent
  , connectTo
  , close
  ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (cons, empty, isInfixOf, unpack)
import           Periodic.Connection   (Connection, newClientConn, receive,
                                        send)

import qualified Periodic.Connection   as Conn (close)
import           Periodic.Types        (ClientType, Payload (..), nullChar)

import           Control.Exception     (bracket, bracketOnError, throwIO)
import qualified Control.Exception     as Exception
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM (delete, empty, insert, lookup)
import           Data.IORef            (IORef, atomicModifyIORef', newIORef)
import           Network               (PortID (..))
import           Network.BSD           (getProtocolNumber)
import           Network.Socket        hiding (close, send)
import qualified Network.Socket        as Socket (close)

import           Periodic.Agent        (Agent (..), feed)
import qualified Periodic.Agent        as Agent (newAgent)
import           System.Entropy        (getEntropy)

import           Periodic.Utils        (parsePayload)

import           Control.Concurrent    (ThreadId, forkIO, killThread)
import           Control.Monad         (forever, liftM, when)

import           Data.Maybe            (fromJust, isJust)

import           System.Log.Logger     (errorM, infoM)

data BaseClient = BaseClient { agents   :: IORef (HashMap ByteString Agent)
                             , conn     :: Connection
                             , threadID :: Maybe ThreadId
                             }

newBaseClient :: Socket -> ClientType -> IO BaseClient
newBaseClient sock agentType = do
  conn' <- newClientConn sock
  send conn' $ (toEnum $ fromEnum agentType) `B.cons` B.empty

  agents' <- newIORef HM.empty
  let bc = BaseClient { conn = conn', agents = agents', threadID = Nothing }

  threadID' <- forkIO $ forever $ mainLoop bc
  infoM "Periodic.BaseClient" "Connected to periodic task system"
  return bc { threadID = Just threadID' }

addAgent :: BaseClient -> Agent -> IO ()
addAgent bc a = atomicModifyIORef' (agents bc) $ \v -> (HM.insert (agentID a) a v, ())

removeAgent :: BaseClient -> Agent -> IO ()
removeAgent bc a = atomicModifyIORef' (agents bc) $ \v -> (HM.delete (agentID a) v, ())

writePayload :: BaseClient -> Payload -> IO ()
writePayload bc pl@(Payload { payloadID = pid }) = do
  v <- atomicModifyIORef' (agents bc) $ \v -> (v, HM.lookup pid v)
  case v of
    Nothing    -> return ()
    Just agent -> feed agent pl

newAgent :: BaseClient -> IO Agent
newAgent bc = do
  aid <- getEntropy 16
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
close (BaseClient { threadID = tid, conn = c }) = do
  Conn.close c
  when (isJust tid) $ killThread (fromJust tid)

mainLoop :: BaseClient -> IO ()
mainLoop bc = do
  rt <- receive (conn bc)
  case rt of
    Left e   -> errorM "Periodic.BaseClient" $ B.unpack e
    Right pl -> writePayload bc (parsePayload pl)


-- Returns the first action from a list which does not throw an exception.
-- If all the actions throw exceptions (and the list of actions is not empty),
-- the last exception is thrown.
-- The operations are run outside of the catchIO cleanup handler because
-- catchIO masks asynchronous exceptions in the cleanup handler.
-- In the case of complete failure, the last exception is actually thrown.
firstSuccessful :: [IO a] -> IO a
firstSuccessful = go Nothing
  where
  -- Attempt the next operation, remember exception on failure
  go _ (p:ps) =
    do r <- tryIO p
       case r of
         Right x -> return x
         Left  e -> go (Just e) ps

  -- All operations failed, throw error if one exists
  go Nothing  [] = error "firstSuccessful: empty list"
  go (Just e) [] = throwIO e


-- | Calling 'connectTo' creates a client side socket which is
-- connected to the given host and port.  The Protocol and socket type is
-- derived from the given port identifier.  If a port number is given
-- then the result is always an internet family 'Stream' socket.
connectTo :: HostName           -- Hostname
          -> PortID             -- Port Identifier
          -> IO Socket          -- Connected Socket

#if defined(IPV6_SOCKET_SUPPORT)
-- IPv6 and IPv4.

connectTo hostname (Service serv) = connect' hostname serv

connectTo hostname (PortNumber port) = connect' hostname (show port)
#else
-- IPv4 only.

connectTo hostname (Service serv) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        (Socket.close)  -- only done if there's an error
        (\sock -> do
          port  <- getServicePortNumber serv
          he    <- getHostByName hostname
          connect sock (SockAddrInet port (hostAddress he))
          return sock
        )

connectTo hostname (PortNumber port) = do
    proto <- getProtocolNumber "tcp"
    bracketOnError
        (socket AF_INET Stream proto)
        (Socket.close)  -- only done if there's an error
        (\sock -> do
          he <- getHostByName hostname
          connect sock (SockAddrInet port (hostAddress he))
          return sock
        )
#endif


#if !defined(mingw32_HOST_OS)
connectTo _ (UnixSocket path) = do
    bracketOnError
        (socket AF_UNIX Stream 0)
        (Socket.close)
        (\sock -> do
          connect sock (SockAddrUnix path)
          return sock
        )
#endif

#if defined(IPV6_SOCKET_SUPPORT)
connect' :: HostName -> ServiceName -> IO Socket

connect' host serv = do
    proto <- getProtocolNumber "tcp"
    let hints = defaultHints { addrFlags = [AI_ADDRCONFIG]
                             , addrProtocol = proto
                             , addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just serv)
    firstSuccessful $ map tryToConnect addrs
  where
  tryToConnect addr =
    bracketOnError
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        (Socket.close)  -- only done if there's an error
        (\sock -> do
          connect sock (addrAddress addr)
          return sock
        )
#endif

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#if MIN_VERSION_base(4,0,0)
catchIO = Exception.catch
#else
catchIO = Exception.catchJust Exception.ioErrors
#endif

-- Version of try implemented in terms of the locally defined catchIO
tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO m = catchIO (liftM Right m) (return . Left)
