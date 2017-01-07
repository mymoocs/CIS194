{--
  Created       : 2017 Jan 05 (Thu) 04:44:16 PM by Arthur Vardanyan.
  Last Modified : 2017 Jan 07 (Sat) 05:47:37 PM by Arthur Vardanyan.
--}

module Cis194.Hw11.SExprTest
           (week11Tests)
    where

import Test.Tasty        (defaultMain, testGroup, TestTree)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Cis194

import Control.Applicative
import Data.Char


week11Tests :: TestTree
week11Tests = testGroup "week11 tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

qcProps :: TestTree
qcProps  = testGroup "(checked by QuickCheck)"
           [

           ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
          []


unitTests :: TestTree
unitTests = testGroup "Unit tests"
            [ testGroup "nondeterministic arithmetic"
              [ -- testCase "Employee"   testEmp
               testCase "(either 4 or 5) times 2, plus either 6 or 1" testEither
              , testCase "possibly-failing arithmetic" testFailures1
              , testCase "possibly-failing arithmetic" testFailures2
              ]
            , testGroup "ex1/zeroOrMore-oneOrMore"
              [ testCase "oneOrMany1"  testSome1
              , testCase "oneOrMany2"  testSome2
              , testCase "zeroOrMany1" testMany1
              , testCase "zeroOrMany2" testMany2
              ]
            , testGroup "ex2/ident/space"
              [ testCase "ident1"   testIdent1
              , testCase "ident2"   testIdent2
              , testCase "ident3"   testIdent3
              , testCase "ident4"   testIdent4
              , testCase "spaces"   testSpaces
              ]
            , testGroup "ex3/S-Expression"
              [ testCase "S-Expr 01"   testSexpr1
              , testCase "S-Expr 02"   testSexpr2
              , testCase "S-Expr 03"   testSexpr3
              , testCase "S-Expr 04"   testSexpr4
              , testCase "S-Expr 05"   testSexpr5
              , testCase "S-Expr 06"   testSexpr6
              , testCase "S-Expr 07"   testSexpr7
              ]
            ]

main = defaultMain week11Tests

-- Lecture samples of nondeterministic arithmetic
-- testEmp = (Emp <$> ["A", "B"] <*> ["1", "2"]) @?= [Emp "A" "1",Emp "A" "2",Emp "B" "1",Emp "B" "2"]
testEither = ([4,5] .* pure 2) .+ [6,1] @?= [14,9,16,11]
testFailures1 = (Just 3 .+ Just 5) .* Just 8 @?= Just 64
testFailures2 = (Just 3 .+ Nothing) .* Just 8 @?= Nothing


-- Exercise 1 / some/many
testMany1 = (runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH") @?= (Just ("ABC","dEfgH"))
testSome1 = (runParser (oneOrMore  (satisfy isUpper)) "ABCdEfgH") @?= (Just ("ABC","dEfgH"))
testMany2 = (runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh") @?= (Just ("","abcdeFGh"))
testSome2 = (runParser (oneOrMore  (satisfy isUpper)) "abcdeFGh") @?= Nothing

-- Exercise 2 / ident/spaces
testIdent1 = (runParser ident  "foobar baz") @?= (Just ("foobar"," baz"))
testIdent2 = (runParser ident  "foo33fA")    @?= (Just ("foo33fA",""))
testIdent3 = (runParser ident  "2bad")       @?= Nothing
testIdent4 = (runParser ident  "")           @?= Nothing
testSpaces = (runParser spaces "   hello")   @?= (Just ("   ","hello"))

-------------------------------------------------------------------------------
-- Exercise 3 / Parsing S-expressions
-------------------------------------------------------------------------------

testSexpr1 = (runParser parseSExpr "5") @?= (Just (A (N 5),""))
testSexpr2 = (runParser parseSExpr "foo3") @?= (Just (A (I "foo3"),""))
testSexpr3 = (runParser parseSExpr "foo bar") @?= (Just (A (I "foo"),"bar"))
testSexpr4 = (runParser parseSExpr "(foo)") @?= (Just (Comb [A (I "foo")],""))
testSexpr5 = (runParser parseSExpr "(foo bar)") @?= (Just (Comb [A (I "foo"),A (I "bar")],""))
testSexpr6 = (runParser parseSExpr "(bar (foo) 3 5 874)") @?=
                  (Just (Comb [A (I "bar"),Comb [A (I "foo")],A (N 3),A (N 5),A (N 874)],""))
testSexpr7 = (runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)") @?=
                  (Just (Comb [Comb [Comb [A (I "lambda"),A (I "x"),Comb [A (I "lambda"),A (I "y"),Comb [A (I "plus"),A (I "x"),A (I "y")]]],A (N 3)],A (N 5)],""))
testSexpr8 = (runParser parseSExpr "( lots of ( spaces in ) this ( one ) )") @?=
                  (Just (Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]],""))
