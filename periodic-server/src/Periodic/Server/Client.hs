{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Periodic.Server.Client
  (
    Client
  , newClient
  , cClose
  ) where

import           Control.Concurrent        (ThreadId, forkIO, killThread)
import           Control.Exception         (try)
import           Control.Monad             (forever, when)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as B (concat, empty, intercalate,
                                                 pack)
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import           Data.Maybe                (fromJust, isJust)
import           Periodic.Connection       (Connection, close, receive)

import           Periodic.Server.Agent     (Agent, newAgent, send, send_)
import           Periodic.Server.FuncStat  (FuncStat (..))
import           Periodic.Server.Job       (Job, parseJob)
import           Periodic.Server.Scheduler (Scheduler, dropFunc, dumpJob,
                                            pushJob, removeJob, status)

import           Data.Aeson                (FromJSON (..), decode, encode,
                                            object, withObject, (.:), (.=))

import           Periodic.Types            (Command (..), Error (..),
                                            Payload (..))
import           Periodic.Utils            (parsePayload)

import           Data.IORef                (IORef, atomicModifyIORef', newIORef)

data Client = Client { cConn     :: Connection
                     , cSched    :: Scheduler
                     , cThreadID :: IORef (Maybe ThreadId)
                     }

newClient :: Connection -> Scheduler -> IO Client
newClient cConn cSched = do
  cThreadID <- newIORef Nothing
  let c = Client {..}

  threadID <- forkIO $ forever $ mainLoop c
  atomicModifyIORef' cThreadID (\_ -> (Just threadID, ()))
  return c

mainLoop :: Client -> IO ()
mainLoop c@(Client {..}) = do
  e <- try $ receive cConn
  case e of
    Left SocketClosed -> cClose c
    Left _            -> cClose c
    Right pl          -> handlePayload c (parsePayload pl)

handlePayload :: Client -> Payload -> IO ()
handlePayload c (Payload {..}) = go payloadCMD
  where go :: Command -> IO ()
        go SubmitJob = handleSubmitJob sched agent payloadData
        go Status    = handleStatus sched agent
        go Ping      = send agent Pong B.empty
        go DropFunc  = handleDropFunc sched agent payloadData
        go RemoveJob = handleRemoveJob sched agent payloadData
        go Dump      = handleDump sched agent
        go Load      = handleLoad sched payloadData
        go _         = send agent Unknown B.empty

        agent = newAgent payloadID $ cConn c
        sched = cSched c

handleSubmitJob :: Scheduler -> Agent -> ByteString -> IO ()
handleSubmitJob sc ag pl = do
  case parseJob pl of
    Nothing -> send ag Noop B.empty
    Just job -> do
      pushJob sc job
      send ag Success B.empty

handleStatus :: Scheduler -> Agent -> IO ()
handleStatus sc ag = do
  stats <- map go <$> status sc
  send_ ag $ B.intercalate "\n" stats

  where go :: FuncStat -> ByteString
        go (FuncStat {..}) = B.concat [ sFuncName
                                      , ","
                                      , B.pack $ show sWorker
                                      , ","
                                      , B.pack $ show sJob
                                      , ","
                                      , B.pack $ show sProcess
                                      , ","
                                      , B.pack $ show sSchedAt
                                      ]

handleDropFunc :: Scheduler -> Agent -> ByteString -> IO ()
handleDropFunc sc ag pl = do
  dropFunc sc pl
  send ag Success B.empty

handleRemoveJob :: Scheduler -> Agent -> ByteString -> IO ()
handleRemoveJob sc ag pl = do
  case parseJob pl of
    Nothing -> send ag Noop B.empty
    Just job -> do
      removeJob sc job
      send ag Success B.empty

handleDump :: Scheduler -> Agent -> IO ()
handleDump sc ag = do
  jobs <- dumpJob sc
  send_ ag . toStrict . encode $ object [ "jobs" .= jobs ]
  send_ ag "EOF"

data LoadPayload = LoadPayload { lpJobs :: [Job] }

instance FromJSON LoadPayload where
  parseJSON = withObject "LoadPayload" $ \o -> do
    lpJobs <- o .: "jobs"
    return LoadPayload {..}

handleLoad :: Scheduler -> ByteString -> IO ()
handleLoad sc pl = do
  case decode (fromStrict pl) of
    Nothing -> return ()
    Just (LoadPayload {..}) -> do
      mapM_ (pushJob sc) lpJobs

cClose :: Client -> IO ()
cClose (Client { .. }) = do
  close cConn
  threadID <- atomicModifyIORef' cThreadID (\v -> (v, v))
  when (isJust threadID) $ killThread (fromJust threadID)
