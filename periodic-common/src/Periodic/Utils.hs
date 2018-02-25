module Periodic.Utils
  (
    makeHeader
  , parseHeader
  , maxLength
  , tryIO
  , getEpochTime
  ) where

import           Data.Bits             (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString.Char8 as B

import           Control.Exception     (IOException, catch)

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

tryIO :: IO a -> IO (Either IOException a)
tryIO m = catch (fmap Right m) (return . Left)

getEpochTime :: IO Int64
getEpochTime = read . show . toEpochTime <$> getUnixTime
