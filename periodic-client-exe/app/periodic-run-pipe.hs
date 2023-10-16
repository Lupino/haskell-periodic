{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forever, replicateM_, void, when)
import           Control.Monad.IO.Class    (liftIO)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as B (drop, hGetSome, hPutStrLn,
                                                 pack, take)
import           Data.Int                  (Int64)
import           Data.List                 (find, isPrefixOf)
import           Data.Maybe                (fromMaybe)
import           Data.Version              (showVersion)
import           Metro.Class               (Transport)
import           Metro.Socket              (getHost, getService)
import           Metro.TP.Socket           (socket)
import           Metro.TP.TLS              (makeClientParams', tlsConfig)
import           Metro.TP.WebSockets       (clientConfig)
import           Metro.TP.XOR              (xorConfig)
import           Paths_periodic_client_exe (version)
import           Periodic.Trans.Job        (JobT, name, schedLater, timeout,
                                            withLock_, workDone_, workFail)
import           Periodic.Trans.Worker     (WorkerT, addFunc, broadcast,
                                            startWorkerT, work)
import           Periodic.Types            (FuncName (..), LockName (..))
import           System.Environment        (getArgs, lookupEnv)
import           System.Exit               (exitSuccess)
import           System.IO                 (hFlush)
import           System.Log.Logger         (errorM)
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
    , xorFile   :: FilePath
    , useTls    :: Bool
    , useWs     :: Bool
    , hostName  :: String
    , certKey   :: FilePath
    , cert      :: FilePath
    , caStore   :: FilePath
    , thread    :: Int
    , lockCount :: Int
    , lockName  :: Maybe LockName
    , notify    :: Bool
    , showHelp  :: Bool
    , timeoutS  :: Int
    , retrySecs :: Int64
    , memLimit  :: Int64
    , bufSize   :: Int
    }

options :: Maybe Int -> Maybe String -> Maybe String -> Options
options t h f = Options
  { host      = fromMaybe "unix:///tmp/periodic.sock" h
  , xorFile   = fromMaybe "" f
  , useTls    = False
  , useWs     = False
  , hostName  = "localhost"
  , certKey   = "client-key.pem"
  , cert      = "client.pem"
  , caStore   = "ca.pem"
  , thread    = fromMaybe 1 t
  , lockCount = 1
  , lockName  = Nothing
  , notify    = False
  , showHelp  = False
  , timeoutS  = -1
  , retrySecs = 0
  , memLimit  = 0
  , bufSize   = 4194304
  }

parseOptions :: [String] -> Options -> (Options, FuncName, String, [String])
parseOptions ("-H":x:xs)           opt = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs)       opt = parseOptions xs opt { host      = x }
parseOptions ("--xor":x:xs)        opt = parseOptions xs opt { xorFile   = x }
parseOptions ("--tls":xs)          opt = parseOptions xs opt { useTls = True }
parseOptions ("--ws":xs)           opt = parseOptions xs opt { useWs = True }
parseOptions ("--hostname":x:xs)   opt = parseOptions xs opt { hostName = x }
parseOptions ("--cert-key":x:xs)   opt = parseOptions xs opt { certKey = x }
parseOptions ("--cert":x:xs)       opt = parseOptions xs opt { cert = x }
parseOptions ("--ca":x:xs)         opt = parseOptions xs opt { caStore = x }
parseOptions ("--thread":x:xs)     opt = parseOptions xs opt { thread = read x }
parseOptions ("--lock-count":x:xs) opt = parseOptions xs opt { lockCount = read x }
parseOptions ("--lock-name":x:xs)  opt = parseOptions xs opt { lockName = Just (LockName $ B.pack x) }
parseOptions ("--help":xs)         opt = parseOptions xs opt { showHelp = True }
parseOptions ("--broadcast":xs)    opt = parseOptions xs opt { notify = True }
parseOptions ("--timeout":x:xs)    opt = parseOptions xs opt { timeoutS = read x }
parseOptions ("--retry-secs":x:xs) opt = parseOptions xs opt { retrySecs = read x }
parseOptions ("--mem-limit":x:xs)  opt = parseOptions xs opt { memLimit = parseMemStr x }
parseOptions ("--buf-size":x:xs)   opt = parseOptions xs opt { bufSize = fromIntegral (parseMemStr x) }
parseOptions ("-h":xs)             opt = parseOptions xs opt { showHelp = True }
parseOptions []                    opt = (opt { showHelp = True }, "", "", [])
parseOptions [_]                   opt = (opt { showHelp = True }, "", "", [])
parseOptions (x:y:xs)              opt = (opt, FuncName $ B.pack x, y, xs)

printHelp :: IO ()
printHelp = do
  putStrLn "periodic-run-pipe - Periodic task system worker"
  putStrLn ""
  putStrLn "Usage: periodic-run-pipe [--host|-H HOST] [--xor FILE] [--ws] [--tls [--hostname HOSTNAME] [--cert-key FILE] [--cert FILE] [--ca FILE]] [--thread THREAD] [--lock-name NAME] [--lock-count COUNT] [--broadcast] [--timeout NSECONDS] [--retry-secs NSECONDS] [--mem-limit MEMORY] [--buf-size SIZE] funcname command [options]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host       Socket path [$PERIODIC_PORT]"
  putStrLn "                  Eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "     --xor        XOR Transport encode file [$XOR_FILE]"
  putStrLn "     --tls        Use tls transport"
  putStrLn "     --ws         Use websockets transport"
  putStrLn "     --hostname   Host name"
  putStrLn "     --cert-key   Private key associated"
  putStrLn "     --cert       Public certificate (X.509 format)"
  putStrLn "     --ca         Trusted certificates"
  putStrLn "     --thread     Worker thread [$THREAD]"
  putStrLn "     --lock-count Max lock count (optional: 1)"
  putStrLn "     --lock-name  The lock name (optional: no lock)"
  putStrLn "     --broadcast  Is broadcast worker"
  putStrLn "     --timeout    Process wait timeout in seconds. use job timeout if net set."
  putStrLn "     --retry-secs Failed job retry in seconds"
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
  f <- lookupEnv "XOR_FILE"
  t <- fmap read <$> lookupEnv "THREAD"

  (opts@Options {..}, func, cmd, argv) <- flip parseOptions (options t h f) <$> getArgs

  when showHelp printHelp

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  run opts func cmd argv

doWork :: Transport tp => Options -> FuncName -> String -> [String] -> WorkerT tp IO ()
doWork opts@Options{..} func cmd argv = do
  pipes <- newTQueueIO
  liftIO $ replicateM_ thread $ void $ createRunner pipes bufSize memLimit cmd argv
  let w = processPipeWorker opts pipes
  if notify then void $ broadcast func w
  else
    case lockName of
      Nothing -> void $ addFunc func w
      Just n  -> void $ addFunc func $ withLock_ n lockCount w
  liftIO $ putStrLn "Worker started."
  work thread

run :: Options -> FuncName -> String -> [String] -> IO ()
run opts@Options {useTls = True, ..} func cmd argv = do
  prms <- makeClientParams' cert [] certKey caStore (hostName, B.pack $ fromMaybe "" $ getService host)
  startWorkerT (tlsConfig prms (socket host)) $ doWork opts func cmd argv

run opts@Options {useWs = True, ..} func cmd argv =
  startWorkerT (clientConfig (socket host) (fromMaybe "0.0.0.0" $ getHost host) (fromMaybe "" $ getService host)) $ doWork opts func cmd argv

run opts@Options {xorFile = "", ..} func cmd argv =
  startWorkerT (socket host) $ doWork opts func cmd argv

run opts@Options {..} func cmd argv =
  startWorkerT (xorConfig xorFile $ socket host) $ doWork opts func cmd argv


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
parseMemMap = map parseMemLine . tail . lines

getMemMap :: IO [(Pid, Int64)]
getMemMap = parseMemMap <$> readProcess "ps" ["-eo", "pid,rss"] ""

getProcessMem :: Pid -> IO Int64
getProcessMem pid = do
  memMap <- getMemMap
  case find ((== pid) . fst) memMap of
    Nothing       -> pure 0
    Just (_, mem) -> pure mem


data Pipe = Pipe
  { pipeIn  :: TMVar ByteString
  , pipeOut :: TMVar ByteString
  , pipeIO  :: TVar (Maybe ProcessHandle)
  }


newPipe :: IO Pipe
newPipe = Pipe <$> newEmptyTMVarIO <*> newEmptyTMVarIO <*> newTVarIO Nothing


createPipeProcess :: Pipe -> Int -> Int64 -> String -> [String] -> IO ()
createPipeProcess Pipe{..} maxBufSize maxMem cmd argv = do
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
        atomically $ writeTVar pipeIO $ Just ph
        io <- async $ forever $ do
          atomically (takeTMVar pipeIn) >>= B.hPutStrLn inh
          hFlush inh
          B.hGetSome outh maxBufSize >>= atomically . tryPutTMVar pipeOut

        io1 <- checkPipeMemory ph onError maxMem

        void $ waitForProcess ph
        cancel io
        mapM_ cancel io1
        void $ atomically $ tryPutTMVar pipeOut "WORKFAIL"


createRunner :: TQueue Pipe -> Int -> Int64 -> String -> [String] -> IO (Async ())
createRunner pipes maxBufSize maxMem cmd argv = do
  pipe <- newPipe
  atomically $ writeTQueue pipes pipe
  async $ forever $ do
    createPipeProcess pipe maxBufSize maxMem cmd argv
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

processPipeWorker :: Transport tp => Options -> TQueue Pipe -> JobT tp IO ()
processPipeWorker Options{..} pipes = do
  Pipe{..} <- readPipe pipes
  n <- name
  atomically $ do
    putTMVar pipeIn n
    void $ tryTakeTMVar pipeOut

  let onError err = errorM "periodic-run-pipe" $ "Task(" ++ show n ++ ") error: " ++ err
  tout <- if timeoutS > -1 then pure timeoutS else timeout
  io <- liftIO $ checkPipeTimeout pipeIO onError tout

  out <- atomically $ takeTMVar pipeOut
  mapM_ cancel io
  case B.take 8 out of
    "WORKDONE" -> void $ workDone_ $ B.drop 9 out
    "WORKFAIL" ->
      if retrySecs > 0 then void $ schedLater retrySecs
                       else void workFail
    _          -> void $ workDone_ out
