{-# LANGUAGE RecordWildCards #-}

module Main
  (
    main
  ) where

import           Periodic.Server
import           System.Environment (getArgs)

data Options = Options { oHost    :: Maybe String
                       , oPort    :: String
                       , oPath    :: String
                       , showHelp :: Bool
                       }

defaultOptions :: Options
defaultOptions = Options { oHost    = Nothing
                         , oPort    = "5000"
                         , oPath    = "state"
                         , showHelp = False
                         }

parseOptions :: [String] -> Options
parseOptions []              = defaultOptions
parseOptions ("-P":x:xs)     = (parseOptions xs) { oPort = x }
parseOptions ("--port":x:xs) = (parseOptions xs) { oPort = x }
parseOptions ("-H":x:xs)     = (parseOptions xs) { oHost = Just x }
parseOptions ("--host":x:xs) = (parseOptions xs) { oHost = Just x }
parseOptions ("-p":x:xs)     = (parseOptions xs) { oPath = x }
parseOptions ("--path":x:xs) = (parseOptions xs) { oPath = x }
parseOptions ("-h":xs)       = (parseOptions xs) { showHelp = True }
parseOptions ("--help":xs)   = (parseOptions xs) { showHelp = True }
parseOptions (x:xs)          = parseOptions xs

printHelp :: IO ()
printHelp = do
  putStrLn "periodicd [--port|-P 5000] [--host|-H 0.0.0.0] [--path|-p state] [--help|-h]"

main :: IO ()
main = do
  (Options {..}) <- parseOptions <$> getArgs

  if showHelp then printHelp
              else startServer oPath oHost oPort
