{--
  Created       : 2016 Nov 22 (Tue) 06:16:23 AM by Arthur Vardanyan.
  Last Modified : 2016 Nov 23 (Wed) 12:39:47 PM by Arthur Vardanyan.
--}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- "orphan instance" - a type class instance C T which is defined in a
-- module which is distinct from both the modules where C and T are defined.


module Cis194.Hw08.PartyTest
           (week8Tests)
    where

import Test.Tasty        (testGroup, TestTree)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Monoid
import Control.Monad

import Cis194

week8Tests :: TestTree
week8Tests = testGroup "week08 tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
            [
             testGroup "Guest List ex. 1"
             [
               testCase "glCons"  $  glFun gl1          @=? 27
             , testCase "empty"   $ (glCons me emptyGL) @=? (GL [me] 10)
             , testCase "glFun"   $ (glFun youAndMeAndThem) @=?  30
             , testCase "length"  $ (length (glGuests youAndMeAndThem)) @=?  3
             , testCase "moreFun" $ (moreFun youAndMe meAndThem) @=?  meAndThem
             ]
            ]
qcProps :: TestTree
qcProps  = testGroup "(checked by QuickCheck)"
           [
            testGroup "Monoid ex. 1"
            [
              QC.testProperty "laws" propMonoidLaws
            , QC.testProperty "fun" propMonoidFun
            , QC.testProperty "ord fun" propOrdFun

            ],
            testGroup "ex. 2"
            [
              testCase "treeFold" $ treeFold ((+) . empFun) (+) 0  testCompany @=? 46
            , testCase "treeFold" $
                  treeFold ((+) . empFun) (+) 0  testCompany2 @=? (9+3+5+1+5+3+17+4)
            ]

           , testCase "nextLevel ex. 3" testNextLevel
           , testCase "maxFun ex. 4" testMaxFun
           ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
          []

-- | SmallCheck

-- | QuickCheck
-------------------------------------------------------------------------------
-- Exercise 1 tests

instance Arbitrary Employee where
  arbitrary = liftM2 Emp arbitrary (liftM abs arbitrary)

instance Arbitrary GuestList where
  arbitrary = liftM2 glCons arbitrary gl where
    gl = frequency [(4, arbitrary), (1, return $ GL [] 0)]

propMonoidLaws :: GuestList -> Bool
propMonoidLaws gl = gl Data.Monoid.<> mempty == gl
                    && mempty Data.Monoid.<> gl == gl

propMonoidFun :: (GuestList,GuestList) -> Bool
propMonoidFun (a, b) = glFun a + glFun b == glFun (a Data.Monoid.<> b)

propOrdFun :: (GuestList,GuestList) -> Bool
propOrdFun (a,b) = (a <= b) == ((glFun a) <= (glFun b))

-- | UnitTest

-------------------------------------------------------------------------------
-- Exercise 3

testNextLevel :: Assertion
testNextLevel = yes == 20 && no == 10 @=? True
    where
      yes = glFun glYes
      no = glFun glNo
      (glYes, glNo) = nextLevel e1 [(gl2,gl3)]

-------------------------------------------------------------------------------
-- Exercise 4


testMaxFun :: Assertion
testMaxFun = (glFun.maxFun $ testCompany) @=? 26



----------------------------------------------

-- | init test data
e1,e2 :: Employee
e1 = Emp "e1" 10
e2 = Emp "e2" 7
e3 = Emp "e3" 4
e4 = Emp "e4" 6

gl0 :: GuestList
gl0 = mempty

gl1 :: GuestList
gl1 = e1 `glCons`
      (e2 `glCons`
      (e3 `glCons`
      (e4 `glCons` gl0)))

gl2 :: GuestList
gl2 = e2 `glCons` gl0

gl3 :: GuestList
gl3 = e3 `glCons` (e4 `glCons` gl0)

-- | Harold Carr test samples
emptyGL :: GuestList
emptyGL = GL [] 0
you  :: Employee
you   = Emp "you" 5
me   :: Employee
me    = Emp "me" 10
them :: Employee
them  = Emp "them" 15

youAndMe        :: GuestList
youAndMe         = glCons you (glCons me   emptyGL)

meAndThem       :: GuestList
meAndThem        = glCons  me (glCons them emptyGL)

youAndMeAndThem :: GuestList
youAndMeAndThem  = youAndMe `mappend` meAndThem


glGuests :: GuestList -> [Employee]
glGuests (GL l _) = l

glFun :: GuestList -> Fun
glFun (GL _ f) = f
