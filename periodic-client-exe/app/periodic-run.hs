{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  ( main
  ) where

import           Control.Concurrent         (forkIO, killThread, threadDelay)
import           Control.DeepSeq            (rnf)
import           Control.Monad              (forever, unless, void, when)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Char8      as B (pack)
import qualified Data.ByteString.Lazy       as LB (null, toStrict)
import qualified Data.ByteString.Lazy.Char8 as LB (hGetContents, hPut)
import           Data.Int                   (Int64)
import           Data.List                  (find, isPrefixOf)
import           Data.Maybe                 (fromMaybe)
import           Data.Version               (showVersion)
import           Metro.Class                (Transport)
import           Metro.Socket               (getHost, getService)
import           Metro.TP.Socket            (socket)
import           Metro.TP.TLS               (makeClientParams', tlsConfig)
import           Metro.TP.WebSockets        (clientConfig)
import           Metro.TP.XOR               (xorConfig)
import           Paths_periodic_client_exe  (version)
import           Periodic.Trans.Job         (JobT, name, schedLater, timeout,
                                             withLock_, workDone, workDone_,
                                             workFail, workload)
import           Periodic.Trans.Worker      (WorkerT, addFunc, broadcast,
                                             startWorkerT, work)
import           Periodic.Types             (FuncName (..), LockName (..))
import           System.Environment         (getArgs, lookupEnv)
import           System.Exit                (ExitCode (..), exitSuccess)
import           System.IO                  (hClose)
import           System.Log.Logger          (errorM)
import           System.Process             (CreateProcess (std_in, std_out),
                                             Pid, ProcessHandle,
                                             StdStream (CreatePipe, Inherit),
                                             getPid, proc, readProcess,
                                             terminateProcess, waitForProcess,
                                             withCreateProcess)
import           UnliftIO                   (MVar, SomeException, async, cancel,
                                             evaluate, mask, newEmptyMVar,
                                             onException, putMVar, takeMVar,
                                             throwIO, try, tryIO)


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
    , useData   :: Bool
    , useName   :: Bool
    , showHelp  :: Bool
    , timeoutS  :: Int
    , retrySecs :: Int64
    , memLimit  :: Int64
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
  , useData   = False
  , useName   = True
  , showHelp  = False
  , timeoutS  = -1
  , retrySecs = 0
  , memLimit  = 0
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
parseOptions ("--data":xs)         opt = parseOptions xs opt { useData = True }
parseOptions ("--no-name":xs)      opt = parseOptions xs opt { useName = False }
parseOptions ("--timeout":x:xs)    opt = parseOptions xs opt { timeoutS = read x }
parseOptions ("--retry-secs":x:xs) opt = parseOptions xs opt { retrySecs = read x }
parseOptions ("--mem-limit":x:xs)  opt = parseOptions xs opt { memLimit = parseMemStr x }
parseOptions ("-h":xs)             opt = parseOptions xs opt { showHelp = True }
parseOptions []                    opt = (opt { showHelp = True }, "", "", [])
parseOptions [_]                   opt = (opt { showHelp = True }, "", "", [])
parseOptions (x:y:xs)              opt = (opt, FuncName $ B.pack x, y, xs)

printHelp :: IO ()
printHelp = do
  putStrLn "periodic-run - Periodic task system worker"
  putStrLn ""
  putStrLn "Usage: periodic-run [--host|-H HOST] [--xor FILE] [--ws] [--tls [--hostname HOSTNAME] [--cert-key FILE] [--cert FILE] [--ca FILE]] [--thread THREAD] [--lock-name NAME] [--lock-count COUNT] [--broadcast] [--data] [--no-name] [--timeout NSECONDS] [--retry-secs NSECONDS] [--mem-limit MEMORY] funcname command [options]"
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
  putStrLn "     --data       Send work data to client"
  putStrLn "     --no-name    Ignore the job name"
  putStrLn "     --timeout    Process wait timeout in seconds. use job timeout if net set."
  putStrLn "     --retry-secs Failed job retry in seconds"
  putStrLn "     --mem-limit  Process max memory limit in bytes (eg. 10k, 1m, 1g, 1024)"
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
  let w = processWorker opts cmd argv
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


processWorker :: Transport tp => Options -> String -> [String] -> JobT tp IO ()
processWorker Options{..} cmd argv = do
  n <- name
  rb <- workload
  tout <- if timeoutS > -1 then pure timeoutS else timeout
  let argv' = if useName then argv ++ [n] else argv
      cp = (proc cmd argv') {std_in = CreatePipe, std_out = if useData then CreatePipe else Inherit}
      onError err = errorM "periodic-run" $ "Task(" ++ n ++ ") error: " ++ err

  (code, out) <- liftIO $ withCreateProcess cp $ \mb_inh mb_outh _ ph ->
    case (mb_inh, mb_outh) of
      (Nothing, _) -> do
        onError "Failed to get a stdin handle."
        terminateProcess ph
        return (ExitFailure 1, Nothing)
      (Just inh, Nothing) -> do
        unless (LB.null rb) $ void $ tryIO $ LB.hPut inh rb
        void $ tryIO $ hClose inh
        code <- finishProcess onError tout memLimit ph
        return (code, Nothing)

      (Just inh, Just outh) -> do
        output  <- LB.hGetContents outh
        withForkWait (evaluate $ rnf output) $ \waitOut -> do
          unless (LB.null rb) $ void $ tryIO $ LB.hPut inh rb
          void $ tryIO $ hClose inh

          code <- finishProcess onError tout memLimit ph
          waitOut
          hClose outh

          return (code, Just output)


  case code of
    ExitFailure _ ->
      if retrySecs > 0 then void $ schedLater retrySecs
                       else void workFail
    ExitSuccess   ->
      case out of
        Nothing -> void workDone
        Just wl -> void $ workDone_ $ LB.toStrict wl

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
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid

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
