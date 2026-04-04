module Main (main) where

import           Control.Monad                (unless)
import           Data.Binary                  (decode, encode)
import qualified Data.ByteString.Lazy         as BL
import           Data.Maybe                   (isNothing)
import           Periodic.Types.WorkerCommand (WorkerCommand (..),
                                               getJobAssignResult)
import           System.Exit                  (exitFailure)

assert :: String -> Bool -> IO ()
assert msg cond = unless cond $ do
  putStrLn $ "FAIL: " ++ msg
  exitFailure

main :: IO ()
main = do
  let rtUnassigned = decode (encode JobUnassigned) :: WorkerCommand
      rtAssigned = decode (encode JobAssigned) :: WorkerCommand
      rtPing = decode (encode Ping) :: WorkerCommand

  assert "JobUnassigned should round-trip by binary codec" $
    rtUnassigned == JobUnassigned
  assert "JobAssigned should round-trip by binary codec" $
    rtAssigned == JobAssigned
  assert "getJobAssignResult JobAssigned should be Just True" $
    getJobAssignResult JobAssigned == Just True
  assert "getJobAssignResult JobUnassigned should be Just False" $
    getJobAssignResult JobUnassigned == Just False
  assert "getJobAssignResult for unrelated command should be Nothing" $
    isNothing (getJobAssignResult rtPing)
  assert "encoded JobUnassigned opcode should be 34" $
    BL.unpack (encode JobUnassigned) == [34]

  putStrLn "periodic-common-test: OK"
