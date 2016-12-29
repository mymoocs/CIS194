{--
Created       : 2016 Nov 17 (Thu) 03:54:49 PM by Arthur Vardanyan.
Last Modified : 2016 Nov 22 (Tue) 06:57:10 PM by Arthur Vardanyan.
--}

module Cis194.Hw07.JoinListTest
           (week7Tests)
    where

import Test.Tasty        (testGroup, TestTree)
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Cis194

week7Tests :: TestTree
week7Tests = testGroup "week07 tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
            [
             testGroup "IndexJ"
             [
               testCase "left" testIndexLeft
             , testCase "right" testIndexRight
             ]
   --         , testCase "e210" $ (same yeah) @?=  (replicate 4 (Right True))

            ]



qcProps :: TestTree
qcProps  = testGroup "(checked by QuickCheck)"
           [
             QC.testProperty "IndexSame" $
               \i -> indexJ i left == indexJ i right
--            , QC.testProperty "DropSame" propDropSame
--           , QC.testProperty "TakeSame" propTakeSame
           ]

scProps :: TestTree
scProps = testGroup "(checked by SmallCheck)"
          []

-- | SmallCheck


-- | QuickCheck
propIndexSame :: Int -> Bool
propIndexSame i = indexJ i left == indexJ i right

{-- }propDropSame :: (Int,Int) -> Bool
propDropSame (x,y) = indexJ x (dropJ y left) == indexJ x (dropJ y right)

propTakeSame :: (Int,Int) -> Bool
propTakeSame (x,y) = indexJ x (takeJ y left) == indexJ x (takeJ y right)
--}
-- | UnitTest
a3,b3,c3,d3,e3 :: JoinList Size String
a3 = Single (Size 1) "a"
b3 = Single (Size 1) "b"
c3 = Single (Size 1) "c"
d3 = Single (Size 1) "d"
e3 = Single (Size 1) "e"


yeah :: JoinList Size Char
yeah = (Single (Size 1) 'y' +++ Single (Size 1) 'e') +++
       (Single (Size 1) 'a' +++ Single (Size 1) 'h')
{-
Append (Size 4)
       (Append (Size 2)
               (Single (Size 1) 'y')  -- 0
               (Single (Size 1) 'e')) -- 1
       (Append (Size 2)
               (Single (Size 1) 'a')  -- 2
               (Single (Size 1) 'h')) -- 3
-}

abcl :: JoinList Size Char
abcl = Append (Size 9)
              (Append (Size 5)
                      (Append (Size 2)
                              (Single (Size 1) 'a')
                              (Single (Size 1) 'b'))
                      (Append (Size 3)
                              (Append (Size 2)
                                      (Single (Size 1) 'c')
                                      (Single (Size 1) 'd'))
                              (Single (Size 1) 'e')))
              (Append (Size 4)
                      (Append (Size 2)
                              (Single (Size 1) 'f')
                              (Single (Size 1) 'g'))
                      (Append (Size 2)
                              (Single (Size 1) 'h')
                              (Single (Size 1) 'i')))

abcr :: JoinList Size Char
abcr = Append (Size 9)
              (Append (Size 4)
                      (Append (Size 2)
                              (Single (Size 1) 'a')
                              (Single (Size 1) 'b'))
                      (Append (Size 3)
                              (Single (Size 1) 'c')
                              (Single (Size 1) 'd')))
              (Append (Size 5)
                      (Append (Size 2)
                              (Single (Size 1) 'e')
                              (Single (Size 1) 'f'))
                      (Append (Size 3)
                              (Single (Size 1) 'g')
                              (Append (Size 2)
                                      (Single (Size 1) 'h')
                                      (Single (Size 1) 'i'))))

{--
gst :: (Sized b, Monoid b) => JoinList b a -> Int
gst = getSize . size . tag

same :: (Eq a, Monoid m, Sized m) => JoinList m a -> [Either (Int, Maybe a, Maybe a) Bool]
same jl = [ go i0 | i0 <- [ 0 .. gst jl - 1] ]
  where
    go i | ij == tl  = Right True
         | otherwise = Left (i, ij, tl)
      where
        ij = indexJ i jl
        tl = toList jl !!? i
--}
left,right :: JoinList Size String

left = ((((a3 +++ b3) +++ c3) +++ d3) +++ e3)

right = (a3 +++ (b3 +++ (c3 +++ (d3 +++ e3))))


testIndexLeft :: Assertion
testIndexLeft = indexJ 2 left @?= Just "c"

testIndexRight :: Assertion
testIndexRight = indexJ 2 right @?= Just "c"

testDropLeft :: Assertion
testDropLeft = dropJ 2 left @?= ((c3 +++ d3) +++ e3)

testDropRight :: Assertion
testDropRight = dropJ 2 right @?= (c3 +++ (d3 +++ e3))

testDropEmpty :: Assertion
testDropEmpty = dropJ 2 (Empty :: JoinList Size String) @?= Empty

testTakeLeft :: Assertion
testTakeLeft = takeJ 3 left @?= ((a3 +++ b3) +++ c3)

testTakeRight :: Assertion
testTakeRight = takeJ 3 right @?= (a3 +++ (b3 +++ c3))

testTakeEmpty :: Assertion
testTakeEmpty = takeJ 2 (Empty :: JoinList Size String) @?= Empty
