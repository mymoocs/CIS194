module Cis194.Hw05UnitTest (hw05Tests) where

import           Cis194.Hw05_Calc
import           Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import           System.Exit (ExitCode(..), exitWith)
import           System.IO.Unsafe (unsafePerformIO)

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

hw05Tests :: IO Counts
hw05Tests =  runTestTT $ TestList
             [ TestList evalTests
             , TestList parseExpTests
             , TestList evalStrTests
             ]


evalTests :: [Test]
evalTests = 
    [ testCase "eval" $
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) @=? 20
    ]

parseExpTests :: [Test]
parseExpTests = 
    [ testCase "evalStr 1" $
      parseExp Lit Add Mul "(2+3)*4" @=? Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    , testCase "evalStr 2" $
      parseExp Lit Add Mul "2+3*4" @=?   Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
    , testCase "evalStr nothing" $
      parseExp Lit Add Mul "2+3*" @=?  Nothing
    ]


evalStrTests :: [Test]
evalStrTests = 
    [ testCase "evalStr 1" $
      (evalStr "(2+3)*4") @=? (Just (eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))))
    , testCase "evalStr 2" $
      (evalStr "2+3*4") @=?  (Just (eval (Add (Lit 2) (Mul (Lit 3) (Lit 4)))))
    , testCase "evalStr nothing" $
      (evalStr "2+3*") @=?   Nothing
    ]
