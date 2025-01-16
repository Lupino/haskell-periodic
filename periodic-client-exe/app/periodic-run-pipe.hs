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
import           UnliftIO                  (Async, MonadIO, TMVar, TQueue, TVar,
                                            async, atomically, cancel,
                                            newEmptyTMVarIO, newTQueueIO,
                                            newTVarIO, putTMVar, readTQueue,
                                            readTVarIO, takeTMVar, tryPutTMVar,
                                            tryTakeTMVar, writeTQueue,
                                            writeTVar)


data Options = Options
  { host      :: String
  , thread    :: Int
  , lockCount :: Int
  , lockName  :: Maybe LockName
  , notify    :: Bool
  , useName   :: Bool
  , showHelp  :: Bool
  , timeoutS  :: Int
  , retrySecs :: Int64
  , memLimit  :: Int64
  , skipFail  :: Bool
  }

options :: Maybe Int -> Maybe String -> Options
options t h = Options
  { host      = fromMaybe "unix:///tmp/periodic.sock" h
  , thread    = fromMaybe 1 t
  , lockCount = 1
  , lockName  = Nothing
  , notify    = False
  , useName   = True
  , showHelp  = False
  , timeoutS  = -1
  , retrySecs = 0
  , memLimit  = 0
  , skipFail  = False
  }

parseOptions :: [String] -> Options -> (Options, FuncName, String, [String])
parseOptions ("-H":x:xs)           opt = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs)       opt = parseOptions xs opt { host      = x }
parseOptions ("--thread":x:xs)     opt = parseOptions xs opt { thread = read x }
parseOptions ("--lock-count":x:xs) opt = parseOptions xs opt { lockCount = read x }
parseOptions ("--lock-name":x:xs)  opt = parseOptions xs opt { lockName = Just (LockName $ B.pack x) }
parseOptions ("--help":xs)         opt = parseOptions xs opt { showHelp = True }
parseOptions ("--broadcast":xs)    opt = parseOptions xs opt { notify = True }
parseOptions ("--no-name":xs)      opt = parseOptions xs opt { useName = False }
parseOptions ("--skip-fail":xs)    opt = parseOptions xs opt { skipFail = True }
parseOptions ("--timeout":x:xs)    opt = parseOptions xs opt { timeoutS = read x }
parseOptions ("--retry-secs":x:xs) opt = parseOptions xs opt { retrySecs = read x }
parseOptions ("--mem-limit":x:xs)  opt = parseOptions xs opt { memLimit = parseMemStr x }
parseOptions ("-h":xs)             opt = parseOptions xs opt { showHelp = True }
parseOptions []                    opt = (opt { showHelp = True }, "", "", [])
parseOptions [_]                   opt = (opt { showHelp = True }, "", "", [])
parseOptions (x:y:xs)              opt = (opt, FuncName $ B.pack x, y, xs)

printHelp :: IO ()
printHelp = do
  putStrLn "periodic-run-pipe - Periodic task system worker"
  putStrLn ""
  putStrLn "Usage: periodic-run-pipe [--host|-H HOST] [--thread THREAD] [--lock-name NAME] [--lock-count COUNT] [--broadcast] [--no-name] [--timeout NSECONDS] [--retry-secs NSECONDS] [--mem-limit MEMORY] [--buf-size SIZE] funcname command [options]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host       Socket path [$PERIODIC_PORT]"
  putStrLn "                  Eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "     --thread     Worker thread [$THREAD]"
  putStrLn "     --lock-count Max lock count (optional: 1)"
  putStrLn "     --lock-name  The lock name (optional: no lock)"
  putStrLn "     --broadcast  Is broadcast worker"
  putStrLn "     --no-name    Use one line workload instead of name"
  putStrLn "     --timeout    Process wait timeout in seconds. use job timeout if net set."
  putStrLn "     --retry-secs Failed job retry in seconds"
  putStrLn "     --skip-fail  Skip failed job, no retry"
  putStrLn "     --mem-limit  Process max memory limit in bytes (eg. 10k, 1m, 1g, 1024)"
  putStrLn "     --buf-size   Pipe max buffer size in bytes (eg. 10k, 1m, 1g, 1024)"
  putStrLn "  -h --help       Display help message"
  putStrLn ""
  putStrLn $ "Version: v" ++ showVersion version
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"
  t <- fmap read <$> lookupEnv "THREAD"

  (opts@Options {..}, func, cmd, argv) <- flip parseOptions (options t h) <$> getArgs

  when showHelp printHelp

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  setupLog INFO
  startWorkerT (socket host) $ doWork opts func cmd argv

doWork :: Transport tp => Options -> FuncName -> String -> [String] -> WorkerT tp IO ()
doWork opts@Options{..} func cmd argv = do
  pipes <- newTQueueIO
  liftIO $ replicateM_ thread $ void $ createRunner pipes memLimit cmd argv
  let w = processPipeWorker opts pipes
  if notify then void $ broadcast func w
  else
    case lockName of
      Nothing -> void $ addFunc func w
      Just n  -> void $ addFunc func $ withLock_ n lockCount w
  liftIO $ putStrLn "Worker started."
  work thread

parseMemStr :: String -> Int64
parseMemStr [] = 0
parseMemStr [x] = read [x]
parseMemStr str = floor $ go (init str) (last str)
  where go :: String -> Char -> Float
        go num 'k' = read num * 1024
        go num 'm' = read num * 1024 * 1024
        go num 'g' = read num * 1024 * 1024 * 1024
        go num 'K' = read num * 1024
        go num 'M' = read num * 1024 * 1024
        go num 'G' = read num * 1024 * 1024 * 1024
        go num c   = read (num ++ [c])

parseMemLine :: String -> (Pid, Int64)
parseMemLine str = (read x :: Pid, parseMemStr y)
  where [x, y] = words str

parseMemMap :: String -> [(Pid, Int64)]
parseMemMap = map parseMemLine . drop 1 . lines

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
  out <- B.hGetLine outh

  if B.take mlen out == mwid then do
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


createPipeProcess :: Pipe -> Int64 -> String -> [String] -> IO ()
createPipeProcess Pipe{..} maxMem cmd argv = do
  let cp = (proc cmd argv) {std_in = CreatePipe, std_out = CreatePipe}
      onError err = errorM "periodic-run-pipe" $ "Error: " ++ err

  withCreateProcess cp $ \mb_inh mb_outh _ ph ->
    case (mb_inh, mb_outh) of
      (Nothing, _) -> do
        onError "Failed to get a stdin handle."
        terminateProcess ph
      (_, Nothing) -> do
        onError "Failed to get a stdout handle."
        terminateProcess ph

      (Just inh, Just outh) -> do
        welcome <- B.hGetLine outh
        infoM "periodic-run-pipe" $ "Welcome: " ++ B.unpack welcome

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
        void $ atomically $ tryTakeTMVar pipeInWait

        cancel io
        mapM_ cancel io1
        hClose outh
        IOMap.modifyIOMap (const Map.empty) pipeOut


createRunner :: TQueue Pipe -> Int64 -> String -> [String] -> IO (Async ())
createRunner pipes maxMem cmd argv = do
  pipe <- newPipe
  atomically $ writeTQueue pipes pipe
  async $ forever $ do
    createPipeProcess pipe maxMem cmd argv
    threadDelay 1000000 -- 1s


checkPipeTimeout :: TVar (Maybe ProcessHandle) -> (String -> IO ()) -> Int -> IO (Maybe (Async ()))
checkPipeTimeout _ _ 0 = pure Nothing
checkPipeTimeout tIO onError t = do
  io <- async $ do
    threadDelay timeoutUs
    onError $ "timeout after " ++ show t ++ "s"
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
          onError $ "overmemory used(" ++ show currMem ++ ") > max(" ++ show maxMem ++ ")"
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
      Just queue -> readTQueue queue

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

  workPipeOut pipeOut msgid skipFail retrySecs

  IOMap.delete msgid pipeOut

  mapM_ cancel io
