module Periodic.Utils
  (
    makeHeader
  , parseHeader
  , parsePayload
  , maxLength
  , tryIO
  , getEpochTime
  ) where

import           Data.Bits             (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString.Char8 as B (ByteString, breakSubstring, cons,
                                             drop, empty, head, length, null,
                                             tail)

import           Periodic.Types        (Command (..), Payload (..), nullChar,
                                        payload)

import           Control.Exception     (IOException, catch)
import           Control.Monad         (liftM)

import           Data.Int              (Int64)
import           Data.UnixTime         (getUnixTime, toEpochTime)

makeHeader :: Int -> B.ByteString
makeHeader x = c 24 `B.cons` c 16 `B.cons` c 8 `B.cons` c 0 `B.cons` B.empty
  where c :: Int -> Char
        c i = toEnum $ x `shiftR` i .&. 0xff

maxLength :: Int
maxLength = 0x7fffffff

parseHeader :: B.ByteString -> Int
parseHeader = go [24, 16, 8, 0]
  where go :: [Int] -> B.ByteString -> Int
        go [] _     = 0
        go (x:xs) h = fromEnum (B.head h) `shiftL` x .|. go xs (B.tail h)

parsePayload :: B.ByteString -> Payload
parsePayload = go . B.breakSubstring nullChar
  where go :: (B.ByteString, B.ByteString) -> Payload
        go (pid, xs) | B.null xs = payload pid Unknown
                     | otherwise = go1 pid (B.breakSubstring nullChar $ B.drop 2 xs)

        go1 :: B.ByteString -> (B.ByteString, B.ByteString) -> Payload
        go1 pid (x, xs) | B.length x == 1 = (payload pid (cmd x)) { payloadData = trim xs }
                        | otherwise       = (payload pid Noop) { payloadData = x }

        trim :: B.ByteString -> B.ByteString
        trim xs | B.null xs = B.empty
                | otherwise = B.drop 2 xs

        cmd :: B.ByteString -> Command
        cmd bs = if v > maxBound || v < minBound then Unknown
                                                 else toEnum v
          where v = fromEnum $ B.head bs

tryIO :: IO a -> IO (Either IOException a)
tryIO m = catch (liftM Right m) (return . Left)

getEpochTime :: IO Int64
getEpochTime = read . show . toEpochTime <$> getUnixTime
