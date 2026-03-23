{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where


import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forever, replicateM_, void, when)
import           Control.Monad.IO.Class    (liftIO)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as B (drop, hGetLine, hPutStrLn,
                                                 length, null, pack, putStrLn,
                                                 take, unpack)
import           Data.Int                  (Int64)
import           Data.IOMap                (IOMap)
import qualified Data.IOMap                as IOMap
import qualified Data.IOMap.STM            as IOMapS
import           Data.List                 (find, isPrefixOf)
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import           Data.Version              (showVersion)
import           Metro.Class               (Transport)
import qualified Metro.TP.RSA              as RSA (RSAMode (AES), configClient)
import           Metro.TP.Socket           (socket)
import           Metro.Utils               (setupLog)
import           Paths_periodic_client_exe (version)
import           Periodic.Node             (sessionGen)
import           Periodic.Trans.Job        (JobT, name, schedLater, timeout,
                                            withLock_, workData, workDone,
                                            workDone_, workFail, workload)
import           Periodic.Trans.Worker     (WorkerT, addFunc, broadcast,
                                            startWorkerT, work)
import           Periodic.Types            (FuncName (..), LockName (..),
                                            Msgid (..))
import           System.Environment        (getArgs, lookupEnv)
import           System.Exit               (exitSuccess)
import           System.IO                 (Handle, hClose, hFlush)
import           System.Log                (Priority (INFO))
import           System.Log.Logger         (errorM, infoM)
import           System.Process            (CreateProcess (std_in, std_out),
                                            Pid, ProcessHandle,
                                            StdStream (CreatePipe), getPid,
                                            proc, readProcess, terminateProcess,
                                            waitForProcess, withCreateProcess)
import           Text.Read                 (readMaybe)
import qualified UnliftIO                  as U (timeout)
import           UnliftIO                  (Async, MonadIO, TMVar, TQueue, TVar,
                                            async, atomically, cancel, catchAny,
                                            finally, newEmptyTMVarIO,
                                            newTQueueIO, newTVarIO, putTMVar,
                                            readTQueue, readTVarIO, retrySTM,
                                            takeTMVar, tryIO, tryPutTMVar,
                                            tryReadTQueue, tryTakeTMVar,
                                            writeTQueue, writeTVar)


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
  }

options :: Maybe Int -> Maybe String -> Options
options t h = Options
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
  , rsaPublicPath  = "public_key.pem"
  , rsaPrivatePath = ""
  , rsaMode        = RSA.AES
  }

parseOptions :: [String] -> Options -> (Options, FuncName, String, [String])
parseOptions ("-H":x:xs)                 opt = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs)             opt = parseOptions xs opt { host      = x }
parseOptions ("--thread":x:xs)           opt = parseOptions xs opt { thread = safeRead (thread opt) x }
parseOptions ("--lock-count":x:xs)       opt = parseOptions xs opt { lockCount = safeRead (lockCount opt) x }
parseOptions ("--lock-name":x:xs)        opt = parseOptions xs opt { lockName = Just (LockName $ B.pack x) }
parseOptions ("--help":xs)               opt = parseOptions xs opt { showHelp = True }
parseOptions ("--broadcast":xs)          opt = parseOptions xs opt { notify = True }
parseOptions ("--no-name":xs)            opt = parseOptions xs opt { useName = False }
parseOptions ("--skip-fail":xs)          opt = parseOptions xs opt { skipFail = True }
parseOptions ("--timeout":x:xs)          opt = parseOptions xs opt { timeoutS = safeRead (timeoutS opt) x }
parseOptions ("--ready-timeout":x:xs)    opt = parseOptions xs opt { readyTimeoutS = safeRead (readyTimeoutS opt) x }
parseOptions ("--retry-secs":x:xs)       opt = parseOptions xs opt { retrySecs = safeRead (retrySecs opt) x }
parseOptions ("--mem-limit":x:xs)        opt = parseOptions xs opt { memLimit = parseMemStr x }
parseOptions ("--rsa-private-path":x:xs) opt = parseOptions xs opt { rsaPrivatePath = x }
parseOptions ("--rsa-public-path":x:xs)  opt = parseOptions xs opt { rsaPublicPath  = x }
parseOptions ("--rsa-mode":x:xs)         opt = parseOptions xs opt { rsaMode  = safeRead (rsaMode opt) x }
parseOptions ("-h":xs)                   opt = parseOptions xs opt { showHelp = True }
parseOptions []                          opt = (opt { showHelp = True }, "", "", [])
parseOptions [_]                         opt = (opt { showHelp = True }, "", "", [])
parseOptions (x:y:xs)                    opt = (opt, FuncName $ B.pack x, y, xs)

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
  putStrLn "      --rsa-public-path <P> RSA public key file or directory"
  putStrLn "      --rsa-private-path <P>RSA private key file path"
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
  t <- fmap (safeRead 1) <$> lookupEnv "THREAD"

  (opts@Options {..}, func, cmd, argv) <- flip parseOptions (options t h) <$> getArgs

  when showHelp printHelp

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Error: Invalid host " ++ host
    printHelp

  setupLog INFO

  pipes <- newTQueueIO
  liftIO $ replicateM_ thread $ createRunner pipes memLimit readyTimeoutS cmd argv

  case rsaPrivatePath of
    "" -> startWorkerT (socket host) $ doWork pipes opts func
    _ -> do
      genTP <- RSA.configClient rsaMode rsaPrivatePath rsaPublicPath
      startWorkerT (genTP $ socket host) $ doWork pipes opts func

doWork :: Transport tp => TQueue Pipe -> Options -> FuncName -> WorkerT tp IO ()
doWork pipes opts@Options{..} func = do
  if notify then void $ broadcast func w
  else
    case lockName of
      Nothing -> void $ addFunc func w
      Just n  -> void $ addFunc func $ withLock_ n lockCount w
  liftIO $ putStrLn "Pipe Worker started."
  work thread
  where w = processPipeWorker opts pipes

parseMemStr :: String -> Int64
parseMemStr [] = 0
parseMemStr [x] = safeRead 0 [x]
parseMemStr str = floor $ go (init str) (last str)
  where go :: String -> Char -> Float
        go num 'k' = safeRead 0 num * 1024
        go num 'm' = safeRead 0 num * 1024 * 1024
        go num 'g' = safeRead 0 num * 1024 * 1024 * 1024
        go num 'K' = safeRead 0 num * 1024
        go num 'M' = safeRead 0 num * 1024 * 1024
        go num 'G' = safeRead 0 num * 1024 * 1024 * 1024
        go num c   = safeRead 0 (num ++ [c])

parseMemLine :: String -> (Pid, Int64)
parseMemLine str =
  case words str of
    [x, y] -> (safeRead 0 x :: Pid, parseMemStr y)
    _      -> (0, 0)

parseMemMap :: String -> [(Pid, Int64)]
parseMemMap = filter ((> 0) . fst) . map parseMemLine . drop 1 . lines

getMemMap :: IO [(Pid, Int64)]
getMemMap = parseMemMap <$> readProcess "ps" ["-eo", "pid,rss"] ""

getProcessMem :: Pid -> IO Int64
getProcessMem pid = do
  memMap <- getMemMap
  case find ((== pid) . fst) memMap of
    Nothing       -> pure 0
    Just (_, mem) -> pure mem

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

safeRead :: Read a => a -> String -> a
safeRead def s = fromMaybe def $ readMaybe s
