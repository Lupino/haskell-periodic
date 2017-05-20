module Periodic.Utils
  (
    makeHeader
  , parseHeader
  , parsePayload
  ) where

import           Data.Bits             (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString.Char8 as B (ByteString, breakSubstring, cons,
                                             drop, empty, head, length, null,
                                             tail)

import           Periodic.Types        (Command (..), Payload (..), nullChar,
                                        payload)

makeHeader :: Int -> B.ByteString
makeHeader x = c 24 `B.cons` c 16 `B.cons` c 8 `B.cons` c 0 `B.cons` B.empty
  where c :: Int -> Char
        c i = toEnum $ x `shiftR` i .&. 0xff

parseHeader :: B.ByteString -> Int
parseHeader = go [24, 16, 8, 0]
  where go :: [Int] -> B.ByteString -> Int
        go [] _     = 0
        go (x:xs) h = fromEnum (B.head h) `shiftL` x .|. go xs (B.tail h)

parsePayload :: B.ByteString -> Payload
parsePayload = go . B.breakSubstring nullChar
  where go :: (B.ByteString, B.ByteString) -> Payload
        go (pid, xs) | B.null xs = payload pid Unknown
                     | otherwise = go1 pid (B.drop 2 xs)

        go1 :: B.ByteString -> B.ByteString -> Payload
        go1 pid x | B.length x > 3 = (payload pid (cmd x)) { payloadData = B.drop 3 x }
                  | otherwise      = (payload pid Noop) { payloadData = x }


        cmd :: B.ByteString -> Command
        cmd = toEnum . fromEnum . B.head
