{-# LANGUAGE CPP #-}

module Periodic.Exec.Util
  ( safeRead
  , strictReadArg
  , parseMemStr
  , parseMemLine
  , parseMemMap
  , getMemMap
  , getProcessMem
  , isValidHost
  , installShutdownSignalHandlers
  , waitForShutdownRequest
  , exitNow
  ) where

import           Control.Monad          (unless, void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Int               (Int64)
import           Data.List              (find, isPrefixOf)
import           Data.Maybe             (fromMaybe)
import           System.Process         (Pid, readProcess)
import           Text.Read              (readMaybe)
import           UnliftIO               (TVar, atomically, readTVar, retrySTM,
                                         writeTVar)
#ifdef mingw32_HOST_OS
import qualified GHC.ConsoleHandler     as Console
import qualified System.Win32.Process   as Win32
#else
import           System.Exit            (ExitCode (..))
import           System.Posix.Process   (exitImmediately)
import           System.Posix.Signals   (Handler (Catch), installHandler,
                                         sigHUP, sigINT, sigTERM)
#endif

safeRead :: Read a => a -> String -> a
safeRead def "" = def
safeRead def s  = fromMaybe def $ readMaybe s

strictReadArg :: Read a => String -> String -> a
strictReadArg flag raw =
  case readMaybe raw of
    Just v  -> v
    Nothing -> error $ "Invalid value for " ++ flag ++ ": " ++ show raw

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

isValidHost :: String -> Bool
isValidHost h = "tcp" `isPrefixOf` h || "unix" `isPrefixOf` h

installShutdownSignalHandlers :: (String -> IO ()) -> TVar Bool -> IO ()
installShutdownSignalHandlers onSignal shuttingDown = do
  let markShutdown sigName = do
        atomically $ writeTVar shuttingDown True
        onSignal sigName
#ifdef mingw32_HOST_OS
  void $ Console.installHandler $ Console.Catch $ \ev -> do
    markShutdown (show ev)
    pure ()
#else
  void $ installHandler sigHUP (Catch $ markShutdown "HUP") Nothing
  void $ installHandler sigTERM (Catch $ markShutdown "TERM") Nothing
  void $ installHandler sigINT (Catch $ markShutdown "INT") Nothing
#endif

waitForShutdownRequest :: MonadIO m => TVar Bool -> m ()
waitForShutdownRequest shuttingDown = liftIO $ atomically $ do
  done <- readTVar shuttingDown
  unless done retrySTM

exitNow :: IO ()
#ifdef mingw32_HOST_OS
exitNow = Win32.exitProcess 0
#else
exitNow = exitImmediately ExitSuccess
#endif
