module Cis194.Hw03UnitTest ( hw03Tests ) where

import           Cis194.Hw03_Golf
import           Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import           System.Exit (ExitCode(..), exitWith)
import           System.IO.Unsafe (unsafePerformIO)

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

hw03Tests :: IO Counts
hw03Tests =  runTestTT $ TestList
             [ TestList skipsTests
             , TestList histogramTests
             ]

skipsTests :: [Test]
skipsTests = []


histogramTests :: [Test]
histogramTests = 
    [ testCase "histogram0" $
     histogram [3,5] @=? "   * *    \n==========\n0123456789\n"
    ]
