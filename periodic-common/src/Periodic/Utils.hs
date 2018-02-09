module Periodic.Utils
  (
    makeHeader
  , parseHeader
  , maxLength
  , tryIO
  , getEpochTime
  , breakBS
  , breakBS2
  , readBS
  ) where

import           Data.Bits               (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString.Char8   as B

import           Control.Exception       (IOException, catch)

import           Periodic.Types.Internal (nullChar, nullCharLength)

import           Data.Int                (Int64)
import           Data.Maybe              (fromMaybe)
import           Data.UnixTime           (getUnixTime, toEpochTime)
import           Text.Read               (readMaybe)

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

breakBS :: Int -> B.ByteString -> [B.ByteString]
breakBS step bs | B.null bs = []
                | step == 0 = []
                | step == 1 = [bs]
                | otherwise = go step $ B.breakSubstring nullChar bs
  where go :: Int -> (B.ByteString, B.ByteString) -> [B.ByteString]
        go s (x, xs) | B.null xs = [x]
                     | otherwise = x : breakBS (s-1) (trim xs)

        trim :: B.ByteString -> B.ByteString
        trim xs | B.null xs = B.empty
                | otherwise = B.drop nullCharLength xs

breakBS2 :: B.ByteString -> (B.ByteString, B.ByteString)
breakBS2 = go . B.breakSubstring nullChar
  where go :: (B.ByteString, B.ByteString) -> (B.ByteString, B.ByteString)
        go (x, xs) = (x, B.drop nullCharLength xs)

readBS :: (Num a, Read a) => B.ByteString -> a
readBS = fromMaybe 0 . readMaybe . B.unpack
