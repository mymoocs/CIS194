module Cis194.Hw04UnitTest (hw04Tests) where

import           Cis194.Hw04
import           Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import           System.Exit (ExitCode(..), exitWith)
import           System.IO.Unsafe (unsafePerformIO)

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

hw04Tests :: IO Counts
hw04Tests =  runTestTT $ TestList
             [ TestList foldTreeTests
             , TestList xorTests
             , TestList myFoldlTests
             ]


foldTreeTests :: [Test]
foldTreeTests = 
    [ testCase "fold tree" $
      foldTree "ABCDEFGHIJ" @=? Node 3 (Node 2 (Node 0 Leaf 'B' Leaf) 'G' (Node 1 Leaf 'F' (Node 0 Leaf 'C' Leaf))) 'J' (Node 2 (Node 1 Leaf 'D' (Node 0 Leaf 'A' Leaf)) 'I' (Node 1 Leaf 'H' (Node 0 Leaf 'E' Leaf)))
    , testCase "check balance" $
      isBalanced (foldTree "ABCDEFGHIJ") @=? True
    , testCase "check balance 1" $
      isTreeBalanced (foldTree "ABCDEFGHIJ") @=?  True      
    ]

xorTests :: [Test]
xorTests =
    [
      testCase "xor odd True" $
      xor [False, True, False] @=? True
    , testCase "xor even True" $
      xor [False, True, False, False, True] @=?  False
    ]

ls = map' show [1..10]
myFoldlTests :: [Test]
myFoldlTests =
    [
     testCase "" $
     myFoldl (\x y -> concat ["(", x, "+",y,")"]) "0" ls @=?
                 "((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)"
    , testCase "" $
     foldl (\x y -> concat ["(", x, "+",y,")"]) "0" ls  @=?
               "((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)"
    ]
    
