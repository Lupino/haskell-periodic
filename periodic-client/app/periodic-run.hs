{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main
  (
    main
  ) where

import           Control.Monad                  (when)
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.ByteString.Char8          as B (ByteString, pack)
import qualified Data.ByteString.Lazy           as LB (null, readFile, toStrict)
import qualified Data.ByteString.Lazy.Char8     as LB (hPut, lines, putStr)
import           Data.List                      (isPrefixOf)
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T (unpack)
import           Data.Text.Encoding             (decodeUtf8With)
import           Data.Text.Encoding.Error       (ignore)
import           Periodic.Job                   (JobT, name, schedLater,
                                                 workData, workDone, workFail,
                                                 workload)
import           Periodic.Socket                (getService)
import           Periodic.Transport             (Transport)
import           Periodic.Transport.TLS
import           Periodic.Transport.XOR         (makeXORTransport)
import           Periodic.Types.Job             (FuncName (..))
import           Periodic.Worker                (addFunc, broadcast, runWorkerT,
                                                 work)
import           System.Environment             (getArgs, lookupEnv)
import           System.Exit                    (ExitCode (..), exitSuccess)
import           System.IO                      (stderr)
import           System.Process.ByteString.Lazy (readProcessWithExitCode)
import           Text.Read                      (readMaybe)


data Options = Options { host     :: String
                       , xorFile  :: FilePath
                       , useTls   :: Bool
                       , hostName :: String
                       , certKey  :: FilePath
                       , cert     :: FilePath
                       , caStore  :: FilePath
                       , thread   :: Int
                       , notify   :: Bool
                       , data_    :: Bool
                       , stdout_  :: Bool
                       , showHelp :: Bool
                       }

options :: Maybe String -> Maybe String -> Options
options h f = Options { host    = fromMaybe "unix:///tmp/periodic.sock" h
                      , xorFile = fromMaybe "" f
                      , useTls = False
                      , hostName = "localhost"
                      , certKey = "client-key.pem"
                      , cert = "client.pem"
                      , caStore = "ca.pem"
                      , thread = 1
                      , notify = False
                      , data_ = False
                      , stdout_ = True
                      , showHelp = False
                      }

parseOptions :: [String] -> Options -> (Options, String, String, [String])
parseOptions ("-H":x:xs)         opt = parseOptions xs opt { host      = x }
parseOptions ("--host":x:xs)     opt = parseOptions xs opt { host      = x }
parseOptions ("--xor":x:xs)      opt = parseOptions xs opt { xorFile   = x }
parseOptions ("--tls":xs)        opt = parseOptions xs opt { useTls = True }
parseOptions ("--hostname":x:xs) opt = parseOptions xs opt { hostName = x }
parseOptions ("--cert-key":x:xs) opt = parseOptions xs opt { certKey = x }
parseOptions ("--cert":x:xs)     opt = parseOptions xs opt { cert = x }
parseOptions ("--ca":x:xs)       opt = parseOptions xs opt { caStore = x }
parseOptions ("--thread":x:xs)   opt = parseOptions xs opt { thread = read x }
parseOptions ("--help":xs)       opt = parseOptions xs opt { showHelp = True }
parseOptions ("--broadcast":xs)  opt = parseOptions xs opt { notify = True }
parseOptions ("--data":xs)       opt = parseOptions xs opt { data_ = True }
parseOptions ("--no-stdout":xs)  opt = parseOptions xs opt { stdout_ = False }
parseOptions ("-h":xs)           opt = parseOptions xs opt { showHelp = True }
parseOptions []                  opt = (opt { showHelp = True }, "", "", [])
parseOptions [_]                 opt = (opt { showHelp = True }, "", "", [])
parseOptions (x:y:xs)            opt = (opt, x, y, xs)

printHelp :: IO ()
printHelp = do
  putStrLn "periodic-run - Periodic task system worker"
  putStrLn ""
  putStrLn "Usage: periodic-run [--host|-H HOST] [--xor FILE|--tls [--hostname HOSTNAME] [--cert-key FILE] [--cert FILE] [--ca FILE] [--broadcast] [--data]] funcname command [options]"
  putStrLn ""
  putStrLn "Available options:"
  putStrLn "  -H --host      Socket path [$PERIODIC_PORT]"
  putStrLn "                 eg: tcp://:5000 (optional: unix:///tmp/periodic.sock) "
  putStrLn "     --xor       XOR Transport encode file [$XOR_FILE]"
  putStrLn "     --tls       Use tls transport"
  putStrLn "     --hostname  Host name"
  putStrLn "     --cert-key  Private key associated"
  putStrLn "     --cert      Public certificate (X.509 format)"
  putStrLn "     --ca        trusted certificates"
  putStrLn "     --thread    worker thread"
  putStrLn "     --broadcast is broadcast worker"
  putStrLn "     --data      send work data to client"
  putStrLn "     --no-stdout hidden the stdout"
  putStrLn "  -h --help      Display help message"
  putStrLn ""
  exitSuccess

main :: IO ()
main = do
  h <- lookupEnv "PERIODIC_PORT"
  f <- lookupEnv "XOR_FILE"

  (opts@Options {..}, func, cmd, argv) <- flip parseOptions (options h f) <$> getArgs

  when showHelp printHelp

  when (not ("tcp" `isPrefixOf` host) && not ("unix" `isPrefixOf` host)) $ do
    putStrLn $ "Invalid host " ++ host
    printHelp

  runWorkerT (makeTransport opts) host $ do
    let proc = if notify then broadcast else addFunc

    proc (FuncName $ B.pack func) $ processWorker stdout_ data_ cmd argv
    liftIO $ putStrLn "Worker started."
    work thread

makeTransport :: Options -> Transport -> IO Transport
makeTransport Options{..} transport =
  if useTls then do
    prms <- makeClientParams' cert [] certKey caStore (hostName, B.pack $ getService host)
    makeTLSTransport prms transport
  else makeTransport' xorFile transport

makeTransport' :: FilePath -> Transport -> IO Transport
makeTransport' [] transport = return transport
makeTransport' p transport  = do
  key <- LB.readFile p
  makeXORTransport key transport

processWorker :: Bool -> Bool -> String -> [String] -> JobT IO ()
processWorker sout dat cmd argv = do
  n <- name
  rb <- workload
  (code, out, err) <- liftIO $ readProcessWithExitCode cmd (argv ++ [n]) rb
  when sout $ liftIO $ LB.putStr out
  liftIO $ LB.hPut stderr err
  case code of
    ExitFailure _ -> workFail
    ExitSuccess | dat  -> workData (LB.toStrict out)
                | LB.null err -> workDone
                | otherwise -> do
      let lastLine = last $ LB.lines err
      case (readMaybe . unpackBS . LB.toStrict) lastLine of
        Nothing    -> workDone
        Just later -> schedLater later

unpackBS :: B.ByteString -> String
unpackBS = T.unpack . decodeUtf8With ignore
