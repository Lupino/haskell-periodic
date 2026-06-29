{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where


import           Control.Concurrent         (forkIO, killThread, threadDelay)
import           Control.Applicative        ((<|>))
import           Control.DeepSeq            (rnf)
import           Control.Monad              (forever, unless, void, when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as B (drop, empty, hGetLine, pack,
                                                  take)
import qualified Data.ByteString.Lazy       as LB (null, toStrict)
import qualified Data.ByteString.Lazy.Char8 as LB (hGetContents, hPut)
import           Data.Int                   (Int64)
import           Data.Maybe                 (fromMaybe)
import           Data.Version               (showVersion)
import           Metro.Class                (Transport)
import qualified Metro.TP.RSA               as RSA (RSAMode (AES), configClient)
import           Metro.TP.Socket            (socket)
import           Paths_periodic_client_exe  (version)
import           Periodic.Exec.Util         (getProcessMem,
                                             installShutdownSignalHandlers,
                                             isValidHost, parseMemStr,
                                             parseReadArg, strictReadArgE)
import           Periodic.Trans.Job         (JobT, name, schedLater, timeout,
                                             withLock_, workData, workDone,
                                             workDone_, workFail, workload)
import           Periodic.Trans.Worker      (WorkerT, addFunc, broadcast,
                                             startWorkerTWithSignalWithAuth,
                                             work)
import           Periodic.Types             (ClientIdentity (ClientIdentity),
                                             FuncName (..), LockName (..))
import           System.Environment         (getArgs, lookupEnv)
import           System.Exit                (ExitCode (..), exitFailure,
                                             exitSuccess)
import           System.IO                  (Handle, hClose)
import           System.Log.Logger          (errorM)
import           System.Process             (CreateProcess (std_in, std_out),
                                             ProcessHandle,
                                             StdStream (CreatePipe, Inherit),
                                             getPid, proc, terminateProcess,
                                             waitForProcess, withCreateProcess)
import           UnliftIO                   (MVar, SomeException, TQueue, TVar,
                                             async, atomically, cancel,
                                             evaluate, mask, newEmptyMVar,
                                             newTQueueIO, newTVarIO,
                                             onException, putMVar, readTQueue,
                                             readTVar, readTVarIO, retrySTM,
                                             takeMVar, throwIO, throwString,
                                             try, tryIO, wait, writeTQueue,
                                             writeTVar)


data Options = Options
  { host           :: String
  , thread         :: Int
  , lockCount      :: Int
  , lockName       :: Maybe LockName
  , notify         :: Bool
  , useData        :: Bool
  , useName        :: Bool
  , showHelp       :: Bool
  , timeoutS       :: Int
  , retrySecs      :: Int64
  , memLimit       :: Int64
  , useWorkData    :: Bool
  , skipFail       :: Bool
  , rsaPrivatePath :: FilePath
  , rsaPublicPath  :: FilePath
  , rsaMode        :: RSA.RSAMode
  , clientName     :: Maybe String
  , clientToken    :: Maybe String
  , parseError     :: Maybe String
  }

options :: Maybe Int -> Maybe String -> Options
options t h = Options
  { host           = fromMaybe "unix:///tmp/periodic.sock" h
  , thread         = fromMaybe 1 t
  , lockCount      = 1
  , lockName       = Nothing
  , notify         = False
  , useData        = False
  , useName        = True
  , showHelp       = False
  , timeoutS       = -1
  , retrySecs      = 0
  , memLimit       = 0
  , useWorkData    = False
  , skipFail       = False
  , rsaPublicPath  = "public_key.pem"
  , rsaPrivatePath = ""
  , rsaMode        = RSA.AES
  , clientName     = Nothing
  , clientToken    = Nothing
  , parseError     = Nothing
  }

parseOptions :: [String] -> Options -> (Options, FuncName, String, [String])
parseOptions ("-H":x:xs)                 opt = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs)             opt = parseOptions xs opt { host      = x }
parseOptions ("--thread":x:xs)           opt = parseOptions xs $ parseReadArg "--thread" x (\v o -> o { thread = v }) onParseErr opt
parseOptions ("--lock-count":x:xs)       opt = parseOptions xs $ parseReadArg "--lock-count" x (\v o -> o { lockCount = v }) onParseErr opt
parseOptions ("--lock-name":x:xs)        opt = parseOptions xs opt { lockName = Just (LockName $ B.pack x) }
parseOptions ("--broadcast":xs)          opt = parseOptions xs opt { notify = True }
parseOptions ("--data":xs)               opt = parseOptions xs opt { useData = True }
parseOptions ("--work-data":xs)          opt = parseOptions xs opt { useWorkData = True }
parseOptions ("--no-name":xs)            opt = parseOptions xs opt { useName = False }
parseOptions ("--skip-fail":xs)          opt = parseOptions xs opt { skipFail = True }
parseOptions ("--timeout":x:xs)          opt = parseOptions xs $ parseReadArg "--timeout" x (\v o -> o { timeoutS = v }) onParseErr opt
parseOptions ("--retry-secs":x:xs)       opt = parseOptions xs $ parseReadArg "--retry-secs" x (\v o -> o { retrySecs = v }) onParseErr opt
parseOptions ("--mem-limit":x:xs)        opt = parseOptions xs opt { memLimit = parseMemStr x }
parseOptions ("--rsa-private-path":x:xs) opt = parseOptions xs opt { rsaPrivatePath = x }
parseOptions ("--rsa-public-path":x:xs)  opt = parseOptions xs opt { rsaPublicPath  = x }
parseOptions ("--rsa-mode":x:xs)         opt = parseOptions xs $ parseReadArg "--rsa-mode" x (\v o -> o { rsaMode = v }) onParseErr opt
parseOptions ("--client-name":x:xs)      opt = parseOptions xs opt { clientName = Just x }
parseOptions ("--client-token":x:xs)     opt = parseOptions xs opt { clientToken = Just x }
parseOptions ("--help":xs)               opt = parseOptions xs opt { showHelp = True }
parseOptions ("-h":xs)                   opt = parseOptions xs opt { showHelp = True }
parseOptions []                          opt = (opt { showHelp = True }, "", "", [])
parseOptions [_]                         opt = (opt { showHelp = True }, "", "", [])
parseOptions (x:y:xs)                    opt = (opt, FuncName $ B.pack x, y, xs)

onParseErr :: String -> Options -> Options
onParseErr err o = o { parseError = parseError o <|> Just err }

printHelp :: IO ()
printHelp = do
  putStrLn "periodic-run - Periodic task system worker"
  putStrLn ""
  putStrLn "Usage: periodic-run [OPTIONS] <funcname> <command> [ARGS...]"
  putStrLn ""
  putStrLn "Global Options:"
  putStrLn "  -H, --host <HOST>         Socket path or address [$PERIODIC_PORT]"
  putStrLn "                            (Default: unix:///tmp/periodic.sock)"
  putStrLn "      --thread <INT>        Max concurrent job threads [$THREAD] (Default: 1)"
  putStrLn ""
  putStrLn "Worker Options:"
  putStrLn "      --lock-name <NAME>    Resource lock name (Default: no lock)"
  putStrLn "      --lock-count <INT>    Max concurrent locks for the named lock (Default: 1)"
  putStrLn "      --broadcast           Register as a broadcast worker"
  putStrLn "      --no-name             Do not pass job name as the last argument"
  putStrLn ""
  putStrLn "Job Control Options:"
  putStrLn "      --timeout <INT>       Process execution timeout (s). Use job default if not set"
  putStrLn "      --retry-secs <INT>    Delay before retrying a failed job (s)"
  putStrLn "      --skip-fail           Do not retry if the process fails"
  putStrLn "      --mem-limit <MEM>     Max memory per process (e.g., 10k, 1m, 1g)"
  putStrLn ""
  putStrLn "Data Handling:"
  putStrLn "      --data                Send process stdout to server as result data"
  putStrLn "      --work-data           Enable WORKDATA/WORKDONE protocol via stdout"
  putStrLn ""
  putStrLn "Security Options:"
  putStrLn "      --rsa-mode <MODE>     RSA mode: Plain, RSA, or AES (Default: AES)"
  putStrLn "      --rsa-public-path <P> RSA public key file or directory"
  putStrLn "      --rsa-private-path <P>RSA private key file path"
  putStrLn "      --client-name <NAME>  Auth client name"
  putStrLn "      --client-token <TOK>  Auth client token"
  putStrLn ""
  putStrLn "Help:"
  putStrLn "  -h, --help                Display this help message"
  putStrLn ""
  putStrLn $ "Version: v" ++ showVersion version
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"
  mtRaw <- lookupEnv "THREAD"
  t <- case mtRaw of
    Nothing -> pure Nothing
    Just raw ->
      case strictReadArgE "THREAD" raw of
        Right v  -> pure $ Just v
        Left err -> putStrLn err >> exitFailure

  (opts@Options {..}, func, cmd, argv) <- flip parseOptions (options t h) <$> getArgs

  when showHelp printHelp
  case parseError of
    Just err -> putStrLn err >> putStrLn "Use --help to see supported options." >> exitFailure
    Nothing  -> pure ()

  unless (isValidHost host) $ do
    putStrLn $ "Error: Invalid host " ++ host
    printHelp

  shuttingDown <- newTVarIO False
  installShutdownSignalHandlers
    (\sig -> errorM "periodic-run" $ "Got signal " ++ sig)
    shuttingDown

  auth <- requireAuthPair opts
  case rsaPrivatePath of
    "" -> startWorkerTWithSignalWithAuth auth (Just shuttingDown) (pure ()) (socket host) $ doWork opts func cmd argv
    _ -> do
      genTP <- RSA.configClient rsaMode rsaPrivatePath rsaPublicPath
      startWorkerTWithSignalWithAuth auth (Just shuttingDown) (pure ()) (genTP $ socket host) $ doWork opts func cmd argv

requireAuthPair :: Options -> IO (Maybe ClientIdentity)
requireAuthPair Options {clientName = Nothing, clientToken = Nothing} = pure Nothing
requireAuthPair Options {clientName = Just n, clientToken = Just t} =
  pure $ Just $ ClientIdentity (B.pack n) (B.pack t)
requireAuthPair _ = do
  putStrLn "Error: --client-name and --client-token must be provided together"
  exitFailure

doWork :: Transport tp => Options -> FuncName -> String -> [String] -> WorkerT tp IO ()
doWork opts@Options{..} func cmd argv = do
  let w = processWorker opts cmd argv
  registered <-
    if notify then broadcast func w
    else
      case lockName of
        Nothing -> addFunc func w
        Just n  -> addFunc func $ withLock_ n lockCount w
  unless registered $
    throwString "worker register failed: server did not accept CanDo/Broadcast"
  liftIO $ putStrLn "Worker started."
  work thread

finishProcess :: (String -> IO ()) ->  Int -> Int64 -> ProcessHandle -> IO ExitCode
finishProcess _ 0 0 ph = waitForProcess ph
finishProcess onError t 0 ph = do
  io <- async $ do
    threadDelay timeoutUs
    onError $ "timeout after " ++ show t ++ "s"
    terminateProcess ph

  retval <- finishProcess onError 0 0 ph
  cancel io

  return retval

  where timeoutUs = t * 1000000

finishProcess onError t mem ph = do
  io <- async $ forever $ do
    threadDelay 1000000 -- 1 seconds
    mpid <- getPid ph
    case mpid of
      Nothing -> pure ()
      Just pid -> do
        currMem <- getProcessMem pid
        when (currMem > mem) $ do
          onError $ "overmemory used(" ++ show currMem ++ ") > max(" ++ show mem ++ ")"
          terminateProcess ph

  retval <- finishProcess onError t 0 ph
  cancel io

  return retval


runPipeOut :: TVar Bool -> TQueue ByteString -> Handle -> IO ()
runPipeOut waiter queue outh = do
  eout <- tryIO $ B.hGetLine outh
  case eout of
    Left _ -> atomically $ writeTVar waiter True
    Right out -> do
      atomically $ writeTQueue queue out

      runPipeOut waiter queue outh


workPipeOut :: Transport tp => TQueue ByteString -> TVar ByteString -> JobT tp IO ()
workPipeOut queue outh = do
  out <- atomically $ readTQueue queue

  case B.take 8 out of
    "WORKDATA" -> do
      void $ workData $ B.drop 9 out
      workPipeOut queue outh
    "WORKDONE" -> atomically $ writeTVar outh $ B.drop 9 out
    _ -> atomically $ do
      buf <- readTVar outh
      writeTVar outh $ buf <> "\n" <> out


processWorker :: Transport tp => Options -> String -> [String] -> JobT tp IO ()
processWorker Options{..} cmd argv = do
  n <- name
  rb <- workload
  tout <- if timeoutS > -1 then pure timeoutS else timeout

  queue <- newTQueueIO
  outTVar <- newTVarIO B.empty

  let argv' = if useName then argv ++ [n] else argv
      cp = (proc cmd argv') {std_in = CreatePipe, std_out = if useData || useWorkData then CreatePipe else Inherit}
      onError err = errorM "periodic-run" $ "Task(" ++ n ++ ") error: " ++ err
      writeIn inh = do
        unless (LB.null rb) $ void $ tryIO $ LB.hPut inh rb
        void $ tryIO $ hClose inh

  mio <- if useWorkData then Just <$> async (workPipeOut queue outTVar) else pure Nothing

  code <- liftIO $ withCreateProcess cp $ \mb_inh mb_outh _ ph ->
    case (mb_inh, mb_outh) of
      (Nothing, _) -> do
        onError "Failed to get a stdin handle."
        terminateProcess ph
        return (ExitFailure 1)
      (Just inh, Nothing) -> do
        writeIn inh
        finishProcess onError tout memLimit ph

      (Just inh, Just outh) -> do
        if useWorkData then do
          writeIn inh
          waiter <- newTVarIO False
          io <- async $ runPipeOut waiter queue outh
          code <- finishProcess onError tout memLimit ph
          atomically $ do
            finished <- readTVar waiter
            unless finished retrySTM

          cancel io
          hClose outh
          return code
        else do
          output  <- LB.hGetContents outh
          withForkWait (evaluate $ rnf output) $ \waitOut -> do
            writeIn inh

            code <- finishProcess onError tout memLimit ph
            waitOut
            hClose outh

            atomically $ writeTVar outTVar $ LB.toStrict output

            return code

  case code of
    ExitFailure _ ->
      if skipFail
        then void workDone
        else
        if retrySecs > 0
          then void $ schedLater retrySecs
          else void workFail
    ExitSuccess   -> do
      mapM_ wait mio
      wl <- readTVarIO outTVar
      void $ workDone_ wl

-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.
--
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait io body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore io) >>= putMVar waitVar
    let wait_ = takeMVar waitVar >>= either throwIO return
    restore (body wait_) `onException` killThread tid
