{-# LANGUAGE OverloadedStrings #-}

import           Periodic.Types
import           Periodic.Utils
import           Test.HUnit

test1 :: Test
test1 = TestCase (assertEqual "for makeHeader 100" "\00\00\03\232" (makeHeader 1000))

test2 :: Test
test2 = TestCase (assertEqual "for parseHeader \00\00\03\232" 1000 (parseHeader "\00\00\03\232"))

test3 :: Test
test3 = TestCase (assertEqual "for parsePayload abc\00\01\02" (payload "abc" SchedLater) (parsePayload "abc\00\01\02"))

tests :: Test
tests = TestList [ TestLabel "test1" test1
                 , TestLabel "test2" test2
                 , TestLabel "test3" test3
                 ]

main :: IO Counts
main = runTestTT tests
