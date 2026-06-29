{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where


import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forever, unless, void, when)
import           Control.Applicative       ((<|>))
import           Control.Monad.IO.Class    (liftIO)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as B (drop, hGetLine, hPutStrLn,
                                                 length, null, pack, putStrLn,
                                                 take, unpack)
import           Data.Int                  (Int64)
import           Data.IOMap                (IOMap)
import qualified Data.IOMap                as IOMap
import qualified Data.IOMap.STM            as IOMapS
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import           Data.Version              (showVersion)
import           Metro.Class               (Transport)
import qualified Metro.TP.RSA              as RSA (RSAMode (AES), configClient)
import           Metro.TP.Socket           (socket)
import           Metro.Utils               (setupLog)
import           Paths_periodic_client_exe (version)
import           Periodic.Exec.Util        (getProcessMem,
                                            installShutdownSignalHandlers,
                                            isValidHost, lookupNonEmptyEnv,
                                            lookupReadEnv, parseMemStr,
                                            parseReadArg, strictReadArgE)
import           Periodic.Node             (sessionGen)
import           Periodic.Trans.Job        (JobT, name, schedLater, timeout,
                                            withLock_, workData, workDone,
                                            workDone_, workFail, workload)
import           Periodic.Trans.Worker     (WorkerT, addFunc, broadcast,
                                            startWorkerTWithSignalWithAuth,
                                            work)
import           Periodic.Types            (ClientIdentity (ClientIdentity),
                                            FuncName (..), LockName (..),
                                            Msgid (..))
import           System.Environment        (getArgs)
import           System.Exit               (exitFailure, exitSuccess)
import           System.IO                 (Handle, hClose, hFlush)
import           System.Log                (Priority (INFO))
import           System.Log.Logger         (errorM, infoM)
import           System.Process            (CreateProcess (std_in, std_out),
                                            ProcessHandle,
                                            StdStream (CreatePipe), getPid,
                                            proc, terminateProcess,
                                            waitForProcess, withCreateProcess)
import qualified UnliftIO                  as U (timeout)
import           UnliftIO                  (Async, MonadIO, TMVar, TQueue, TVar,
                                            async, atomically, cancel, catchAny,
                                            finally, newEmptyTMVarIO,
                                            newTQueueIO, newTVarIO, putTMVar,
                                            readTQueue, readTVarIO, retrySTM,
                                            takeTMVar, throwString, tryIO,
                                            tryPutTMVar, tryReadTQueue,
                                            tryTakeTMVar, writeTQueue,
                                            writeTVar)


data Options = Options
  { host           :: String
  , thread         :: Int
  , lockCount      :: Int
  , lockName       :: Maybe LockName
  , notify         :: Bool
  , useName        :: Bool
  , showHelp       :: Bool
  , timeoutS       :: Int
  , readyTimeoutS  :: Int
  , retrySecs      :: Int64
  , memLimit       :: Int64
  , skipFail       :: Bool
  , rsaPrivatePath :: FilePath
  , rsaPublicPath  :: FilePath
  , rsaMode        :: RSA.RSAMode
  , clientName     :: Maybe String
  , clientToken    :: Maybe String
  , parseError     :: Maybe String
  }

options :: Maybe Int -> Maybe String -> Maybe FilePath -> Maybe FilePath -> RSA.RSAMode -> Maybe String -> Maybe String -> Options
options t h mRsaPrivate mRsaPublic mode mClientName mClientToken = Options
  { host           = fromMaybe "unix:///tmp/periodic.sock" h
  , thread         = fromMaybe 1 t
  , lockCount      = 1
  , lockName       = Nothing
  , notify         = False
  , useName        = True
  , showHelp       = False
  , timeoutS       = -1
  , readyTimeoutS  = 10
  , retrySecs      = 0
  , memLimit       = 0
  , skipFail       = False
  , rsaPublicPath  = fromMaybe "public_key.pem" mRsaPublic
  , rsaPrivatePath = fromMaybe "" mRsaPrivate
  , rsaMode        = mode
  , clientName     = mClientName
  , clientToken    = mClientToken
  , parseError     = Nothing
  }

parseOptions :: [String] -> Options -> (Options, FuncName, String, [String])
parseOptions ("-H":x:xs)                 opt = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs)             opt = parseOptions xs opt { host      = x }
parseOptions ("--thread":x:xs)           opt = parseOptions xs $ parseReadArg "--thread" x (\v o -> o { thread = v }) onParseErr opt
parseOptions ("--lock-count":x:xs)       opt = parseOptions xs $ parseReadArg "--lock-count" x (\v o -> o { lockCount = v }) onParseErr opt
parseOptions ("--lock-name":x:xs)        opt = parseOptions xs opt { lockName = Just (LockName $ B.pack x) }
parseOptions ("--help":xs)               opt = parseOptions xs opt { showHelp = True }
parseOptions ("--broadcast":xs)          opt = parseOptions xs opt { notify = True }
parseOptions ("--no-name":xs)            opt = parseOptions xs opt { useName = False }
parseOptions ("--skip-fail":xs)          opt = parseOptions xs opt { skipFail = True }
parseOptions ("--timeout":x:xs)          opt = parseOptions xs $ parseReadArg "--timeout" x (\v o -> o { timeoutS = v }) onParseErr opt
parseOptions ("--ready-timeout":x:xs)    opt = parseOptions xs $ parseReadArg "--ready-timeout" x (\v o -> o { readyTimeoutS = v }) onParseErr opt
parseOptions ("--retry-secs":x:xs)       opt = parseOptions xs $ parseReadArg "--retry-secs" x (\v o -> o { retrySecs = v }) onParseErr opt
parseOptions ("--mem-limit":x:xs)        opt = parseOptions xs opt { memLimit = parseMemStr x }
parseOptions ("--rsa-private-path":x:xs) opt = parseOptions xs opt { rsaPrivatePath = x }
parseOptions ("--rsa-public-path":x:xs)  opt = parseOptions xs opt { rsaPublicPath  = x }
parseOptions ("--rsa-mode":x:xs)         opt = parseOptions xs $ parseReadArg "--rsa-mode" x (\v o -> o { rsaMode = v }) onParseErr opt
parseOptions ("--client-name":x:xs)      opt = parseOptions xs opt { clientName = Just x }
parseOptions ("--client-token":x:xs)     opt = parseOptions xs opt { clientToken = Just x }
parseOptions ("-h":xs)                   opt = parseOptions xs opt { showHelp = True }
parseOptions []                          opt = (opt { showHelp = True }, "", "", [])
parseOptions [_]                         opt = (opt { showHelp = True }, "", "", [])
parseOptions (x:y:xs)                    opt = (opt, FuncName $ B.pack x, y, xs)

onParseErr :: String -> Options -> Options
onParseErr err o = o { parseError = parseError o <|> Just err }

printHelp :: IO ()
printHelp = do
  putStrLn "periodic-run-pipe - Periodic worker with persistent pipe communication"
  putStrLn ""
  putStrLn "Usage: periodic-run-pipe [OPTIONS] <funcname> <command> [ARGS...]"
  putStrLn ""
  putStrLn "Global Options:"
  putStrLn "  -H, --host <HOST>         Socket path or address [$PERIODIC_PORT]"
  putStrLn "                            (Default: unix:///tmp/periodic.sock)"
  putStrLn "      --thread <INT>        Number of persistent runner processes [$THREAD] (Default: 1)"
  putStrLn ""
  putStrLn "Worker Options:"
  putStrLn "      --lock-name <NAME>    Resource lock name (Default: no lock)"
  putStrLn "      --lock-count <INT>    Max concurrent locks for the named lock (Default: 1)"
  putStrLn "      --broadcast           Register as a broadcast worker (Fan-out mode)"
  putStrLn "      --no-name             Pass workload data instead of job name to the pipe"
  putStrLn ""
  putStrLn "Performance & Control:"
  putStrLn "      --timeout <INT>       Execution timeout (s). Use job default if not set"
  putStrLn "      --ready-timeout <INT> Runner startup ready timeout (s). 0 disables timeout (Default: 10)"
  putStrLn "      --retry-secs <INT>    Delay before retrying a failed job (s)"
  putStrLn "      --skip-fail           Do not retry if the process fails"
  putStrLn "      --mem-limit <MEM>     Max memory per runner (e.g., 1024, 1m, 1g)"
  putStrLn "      --buf-size <MEM>      Max pipe buffer size (Default: 1024)"
  putStrLn ""
  putStrLn "Security Options:"
  putStrLn "      --rsa-mode <MODE>     RSA mode: Plain, RSA, or AES (Default: AES)"
  putStrLn "                            Env: $PERIODIC_RSA_MODE"
  putStrLn "      --rsa-public-path <P> RSA public key file or directory [$PERIODIC_RSA_PUBLIC_PATH]"
  putStrLn "      --rsa-private-path <P>RSA private key file path [$PERIODIC_RSA_PRIVATE_PATH]"
  putStrLn "      --client-name <NAME>  Auth client name [$PERIODIC_CLIENT_NAME]"
  putStrLn "      --client-token <TOK>  Auth client token [$PERIODIC_CLIENT_TOKEN]"
  putStrLn ""
  putStrLn "Help:"
  putStrLn "  -h, --help                Display this help message"
  putStrLn ""
  putStrLn $ "Version: v" ++ showVersion version
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupNonEmptyEnv "PERIODIC_PORT"
  mtRaw <- lookupNonEmptyEnv "THREAD"
  t <- case mtRaw of
    Nothing -> pure Nothing
    Just raw ->
      case strictReadArgE "THREAD" raw of
        Right v  -> pure $ Just v
        Left err -> putStrLn err >> exitFailure

  rsaPrivate <- lookupNonEmptyEnv "PERIODIC_RSA_PRIVATE_PATH"
  rsaPublic <- lookupNonEmptyEnv "PERIODIC_RSA_PUBLIC_PATH"
  rsaModeEnv <- lookupReadEnv "PERIODIC_RSA_MODE" >>= either (\err -> putStrLn err >> exitFailure) (pure . fromMaybe RSA.AES)
  clientNameEnv <- lookupNonEmptyEnv "PERIODIC_CLIENT_NAME"
  clientTokenEnv <- lookupNonEmptyEnv "PERIODIC_CLIENT_TOKEN"

  (opts@Options {..}, func, cmd, argv) <- flip parseOptions (options t h rsaPrivate rsaPublic rsaModeEnv clientNameEnv clientTokenEnv) <$> getArgs

  when showHelp printHelp
  case parseError of
    Just err -> putStrLn err >> putStrLn "Use --help to see supported options." >> exitFailure
    Nothing  -> pure ()

  unless (isValidHost host) $ do
    putStrLn $ "Error: Invalid host " ++ host
    printHelp

  setupLog INFO

  shuttingDown <- newTVarIO False

  installShutdownSignalHandlers
    (\sig -> infoM "periodic-run-pipe" $ "Got signal " ++ sig)
    shuttingDown

  pipes <- newTQueueIO
  runners <- liftIO $ mapM (\_ -> createRunner pipes memLimit readyTimeoutS cmd argv) [1 .. thread]

  auth <- requireAuthPair opts
  case rsaPrivatePath of
    "" -> startWorkerTWithSignalWithAuth auth (Just shuttingDown) (cleanupPipes pipes runners) (socket host) $ doWork pipes opts func
    _ -> do
      genTP <- RSA.configClient rsaMode rsaPrivatePath rsaPublicPath
      startWorkerTWithSignalWithAuth auth (Just shuttingDown) (cleanupPipes pipes runners) (genTP $ socket host) $ doWork pipes opts func

requireAuthPair :: Options -> IO (Maybe ClientIdentity)
requireAuthPair Options {clientName = Nothing, clientToken = Nothing} = pure Nothing
requireAuthPair Options {clientName = Just n, clientToken = Just t} =
  pure $ Just $ ClientIdentity (B.pack n) (B.pack t)
requireAuthPair _ = do
  putStrLn "Error: --client-name and --client-token must be provided together"
  exitFailure

doWork :: Transport tp => TQueue Pipe -> Options -> FuncName -> WorkerT tp IO ()
doWork pipes opts@Options{..} func = do
  let w = processPipeWorker opts pipes
  registered <-
    if notify then broadcast func w
    else
      case lockName of
        Nothing -> addFunc func w
        Just n  -> addFunc func $ withLock_ n lockCount w
  unless registered $
    throwString "pipe worker register failed: server did not accept CanDo/Broadcast"
  liftIO $ putStrLn "Pipe Worker started."

  work thread

cleanupPipes :: TQueue Pipe -> [Async ()] -> IO ()
cleanupPipes pipes runners = do
  stopPipes pipes
  mapM_ cancel runners

type PipeOut = IOMap Msgid (TQueue ByteString)

data Pipe = Pipe
  { pipeInWait :: TMVar Bool
  , pipeIn     :: TMVar (Msgid, ByteString)
  , pipeOut    :: PipeOut
  , pipeIO     :: TVar (Maybe ProcessHandle)
  }


newPipe :: IO Pipe
newPipe =
  Pipe
    <$> newEmptyTMVarIO
    <*> newEmptyTMVarIO
    <*> IOMap.empty
    <*> newTVarIO Nothing


runPipeOut :: PipeOut -> Handle -> Msgid -> IO ()
runPipeOut pipeout outh msgid@(Msgid wid) = do
  eout <- tryIO $ B.hGetLine outh
  case eout of
    Left _ -> pure ()
    Right out -> do
      if B.take (mlen + 1) out == mwid <> " " then do
        let dat = B.drop (mlen + 1) out
        atomically $ do
          mqueue <- IOMapS.lookup msgid pipeout
          mapM_ (`writeTQueue` dat) mqueue
        case B.take 8 dat of
          "WORKDATA" -> do
            runPipeOut pipeout outh msgid
          _ -> pure ()
      else do
        B.putStrLn out
        when (B.null out) $ threadDelay 10000 -- 10ms
        runPipeOut pipeout outh msgid

  where mwid = B.pack (show wid)
        mlen = B.length mwid


createPipeProcess :: Pipe -> Int64 -> Int -> String -> [String] -> IO ()
createPipeProcess Pipe{..} maxMem readyTout cmd argv = do
  atomically $ writeTVar pipeIO Nothing
  let cp = (proc cmd argv) {std_in = CreatePipe, std_out = CreatePipe}
      onError err = errorM "periodic-run-pipe" $ "Process Error: " ++ err

  withCreateProcess cp $ \mb_inh mb_outh _ ph ->
    case (mb_inh, mb_outh) of
      (Nothing, _) -> do
        onError "Failed to get a stdin handle."
        terminateProcess ph
      (_, Nothing) -> do
        onError "Failed to get a stdout handle."
        terminateProcess ph

      (Just inh, Just outh) -> do
        mWelcome <- if readyTout <= 0
          then Just <$> tryIO (B.hGetLine outh)
          else U.timeout (readyTout * 1000000) (tryIO $ B.hGetLine outh)
        case mWelcome of
          Nothing -> do
            onError $ "Runner startup timeout after " ++ show readyTout ++ "s waiting for ready line"
            terminateProcess ph
          Just (Left _) -> do
            onError "Runner failed before ready line was received"
            terminateProcess ph
          Just (Right welcome) -> do
            infoM "periodic-run-pipe" $ "Pipe Runner Ready: " ++ B.unpack welcome

            atomically $ writeTVar pipeIO $ Just ph
            io <- async $ forever $ do

              void $ atomically $ tryPutTMVar pipeInWait True
              (msgid@(Msgid wid), bs) <- atomically $ takeTMVar pipeIn

              let mwid = B.pack (show wid)

              B.hPutStrLn inh (mwid <> " " <> bs)
              hFlush inh

              runPipeOut pipeOut outh msgid

            io1 <- checkPipeMemory ph onError maxMem

            void $ waitForProcess ph
            atomically $ writeTVar pipeIO Nothing
            void $ atomically $ tryTakeTMVar pipeInWait

            cancel io
            mapM_ cancel io1
            hClose outh
            IOMap.modifyIOMap (const Map.empty) pipeOut


createRunner :: TQueue Pipe -> Int64 -> Int -> String -> [String] -> IO (Async ())
createRunner pipes maxMem readyTout cmd argv = do
  pipe <- newPipe
  atomically $ writeTQueue pipes pipe
  async $ forever $ do
    catchAny
      (createPipeProcess pipe maxMem readyTout cmd argv)
      (\e -> errorM "periodic-run-pipe" $ "Runner crashed: " ++ show e)
    threadDelay 1000000 -- 1s


checkPipeTimeout :: TVar (Maybe ProcessHandle) -> (String -> IO ()) -> Int -> IO (Maybe (Async ()))
checkPipeTimeout _ _ 0 = pure Nothing
checkPipeTimeout tIO onError t = do
  io <- async $ do
    threadDelay timeoutUs
    onError $ "Execution timeout after " ++ show t ++ "s"
    io <- readTVarIO tIO
    mapM_ terminateProcess io

  pure $ Just io
  where timeoutUs = t * 1000000


checkPipeMemory :: ProcessHandle -> (String -> IO ()) -> Int64 -> IO (Maybe (Async ()))
checkPipeMemory _ _ 0         = pure Nothing
checkPipeMemory ph onError maxMem = do
  mpid <- getPid ph
  case mpid of
    Nothing  -> pure Nothing
    Just pid -> do
      nio <- async $ forever $ do
        threadDelay 1000000 -- 1 seconds
        currMem <- getProcessMem pid
        when (currMem > maxMem) $ do
          onError $ "Memory limit exceeded: used(" ++ show currMem ++ ") > max(" ++ show maxMem ++ ")"
          terminateProcess ph
      return $ Just nio


readPipe :: MonadIO m => TQueue Pipe -> m Pipe
readPipe pipes = liftIO $ atomically $ do
  pipe <- readTQueue pipes
  writeTQueue pipes pipe
  return pipe


workPipeOut :: Transport tp => PipeOut -> Msgid -> Bool -> Int64 -> JobT tp IO ()
workPipeOut pipeout msgid skipFail retrySecs = do
  out <- atomically $ do
    mqueue <- IOMapS.lookup msgid pipeout
    case mqueue of
      Nothing    -> pure "WORKFAIL"
      Just queue -> do
        mOut <- tryReadTQueue queue
        case mOut of
          Just v  -> pure v
          Nothing -> do
            mqueue' <- IOMapS.lookup msgid pipeout
            case mqueue' of
              Nothing -> pure "WORKFAIL"
              Just _  -> retrySTM

  case B.take 8 out of
    "WORKDATA" -> do
      void $ workData $ B.drop 9 out
      workPipeOut pipeout msgid skipFail retrySecs
    "WORKDONE" -> void $ workDone_ $ B.drop 9 out
    "WORKFAIL" ->
        if skipFail
          then void workDone
          else
          if retrySecs > 0
            then void $ schedLater retrySecs
            else void workFail
    _          -> void $ workDone_ out


processPipeWorker :: Transport tp => Options -> TQueue Pipe -> JobT tp IO ()
processPipeWorker Options{..} pipes = do
  Pipe{..} <- readPipe pipes
  jn <- name
  n <- if useName then name else workload
  msgid <- liftIO sessionGen
  queue <- newTQueueIO
  atomically $ do
    void $ takeTMVar pipeInWait
    putTMVar pipeIn (msgid, n)
    IOMapS.insert msgid queue pipeOut

  let onError err = errorM "periodic-run-pipe" $ "Task(" ++ jn ++ ") error: " ++ err
  tout <- if timeoutS > -1 then pure timeoutS else timeout
  io <- liftIO $ checkPipeTimeout pipeIO onError tout

  workPipeOut pipeOut msgid skipFail retrySecs `finally` do
    IOMap.delete msgid pipeOut
    mapM_ cancel io

stopPipes :: TQueue Pipe -> IO ()
stopPipes pipes = do
  allPipes <- atomically $ drainQueue pipes []
  mapM_ stopPipe allPipes

drainQueue pipes acc = do
  mp <- tryReadTQueue pipes
  case mp of
    Nothing -> pure acc
    Just p  -> drainQueue pipes (p : acc)

stopPipe :: Pipe -> IO ()
stopPipe Pipe{..} = do
  mph <- readTVarIO pipeIO
  mapM_ terminateProcess mph
