module Main
  (
    main
  ) where

import           Network            (PortID (..))
import           Periodic.Server
import           System.Environment (getArgs)

defaultPort :: String
defaultPort = "5000"

defaultStorePath :: String
defaultStorePath = "state"

main :: IO ()
main = do
  args <- getArgs

  let (path, host, port) = case args of
                             [x]    -> (defaultStorePath, Nothing, x)
                             [x, y] -> (defaultStorePath, Just x, y)
                             [x, y, z] -> (x, Just y, z)
                             _      -> (defaultStorePath, Nothing, defaultPort)


  startServer path host port

