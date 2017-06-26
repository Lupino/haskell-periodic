{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.BaseClient
  (
    BaseClient
  , HostName
  , ServiceName
  , newBaseClient
  , removeAgent
  , newAgent
  , withAgent
  , noopAgent
  , connectTo
  , connectToFile
  , close
  ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (cons, empty, isInfixOf, unpack)
import           Periodic.Connection   (Connection, newClientConn, receive,
                                        send)

import qualified Periodic.Connection   as Conn (close)
import           Periodic.Types        (ClientType, Error (..), Payload (..),
                                        noopError, nullChar)

import           Control.Exception     (bracket, bracketOnError, throwIO, try)
import qualified Control.Exception     as Exception
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM (delete, elems, empty, insert,
                                              lookup)
import           Data.IORef            (IORef, atomicModifyIORef', newIORef)
import           Network               (PortID (..))
import           Network.BSD           (getProtocolNumber)
import           Network.Socket        hiding (close, send)
import qualified Network.Socket        as Socket (close)

import           Periodic.Agent        (Agent, agentID, feed)
import qualified Periodic.Agent        as Agent (newAgent)
import           System.Entropy        (getEntropy)

import           Periodic.Utils        (parsePayload)

import           Control.Concurrent    (ThreadId, forkIO, killThread)
import           Control.Monad         (forever, liftM, when)

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

  agents <- newIORef HM.empty
  threadID <- newIORef Nothing
  let bc = BaseClient {..}

  t <- forkIO $ forever $ mainLoop bc
  atomicModifyIORef' threadID (\v -> (Just t, ()))
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
    Right pl           -> writePayload bc (parsePayload pl)


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


connectTo :: HostName -> ServiceName -> IO Socket

connectTo host serv = do
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
          setSocketOption sock KeepAlive 1
          connect sock (addrAddress addr)
          return sock
        )

-- Version of try implemented in terms of the locally defined catchIO
tryIO :: IO a -> IO (Either Exception.IOException a)
tryIO m = Exception.catch (liftM Right m) (return . Left)

connectToFile :: FilePath -> IO Socket
connectToFile path = do
  bracketOnError
    (socket AF_UNIX Stream 0)
    (sClose)
    (\sock -> do
      setSocketOption sock KeepAlive 1
      connect sock (SockAddrUnix path)
      return sock
    )
