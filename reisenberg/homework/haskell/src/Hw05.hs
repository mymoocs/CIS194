{-# OPTIONS_GHC -Wall #-}

module Hw05 where
import Hw05_Ring
import Hw05_Parser
import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U 

import Data.List (stripPrefix)
import Data.Maybe (listToMaybe)
--------------------------------------------------------------------------------
--- Rings
--------------------------------------------------------------------------------


{- class Ring a where
  addId :: a -- additive identity
  addInv :: a -> a -- additive inverse
  mulId :: a -- multiplicative identity
  add :: a -> a -> a -- addition
  mul :: a -> a -> a -- multiplication

-}
-------------------------------------------------------------------------------
--- Exercise 1. HUnit testing

intParsingWorks :: Bool
intParsingWorks =    (parse "3" == Just (3 :: Integer, ""))
                  && (parseRing "1+2*5" == Just (11 :: Integer))
                  && (addId == (0 :: Integer))  

parseTests :: T.Test
parseTests = T.TestList
             [
               U.teq "ex10" (parse "3")         (Just (3 :: Integer, ""))
             , U.teq "ex11" (parseRing "1+2*5") ( Just (11 :: Integer))
             , U.teq "ex12" addId               (0 :: Integer)
             , U.teq "ex13" (parse "True")      (Just (True, ""))
             ]

-------------------------------------------------------------------------------
--- Exercise 2.Modular arithmetic forms a ring

-- |
-- We will be thinking of the integers modulo 5.
-- This ring has 5 elements: R={0, 1, 2, 3, 4}.
-- 3 + 4 = 2
-- 1 + 4 = 0
data Mod5 = MkMod Integer
          deriving (Show, Eq)

instance Parsable Mod5 where
  parse str
    | Just (i, rest) <- parse str = Just (MkMod (mod i 5), rest)
    | otherwise = Nothing

instance Ring Mod5 where
  addId  = MkMod 0
--  addInv = id
  mulId  = MkMod 1

  add (MkMod i1) (MkMod i2) = MkMod (mod (i1 + i2) 5) 
  mul (MkMod i1) (MkMod i2) = MkMod (i1 * i2)

ex2 :: T.Test
ex2 = T.TestList
      [
        U.teq "ex20" (parse "3")         (Just (MkMod 3, ""))
      , U.teq "ex21" (parse "13")        (Just (MkMod 3, ""))
      , U.teq "ex22" (parseRing "1+2+5") (Just (MkMod 3))
      , U.teq "ex23" (parseRing "1+2*5") (Just (MkMod 1))        
      ]


--------------------------------------------------------------------------------
--- Exercise 3.
-- |
-- Matrix arithmetic forms a ring. Write a datatype Mat2x2
-- (you choose the representation) and Ring and Parsable instances.
-- Your parser must be able to read something like
-- [[1,2][3,4]] as a 2x2 matrix.

data Mat2x2 = Mat [[Int]]
              deriving (Show, Eq)

-- instance Parsable Mat2x2 where

instance Ring Mat2x2 where
  addId  = Mat [[0,0],[0,0]] 
--  addInv = id
  mulId  = Mat [[1,0],[0,1]]

  add (Mat [[x11,x12],[x21, x22]]) (Mat [[y11,y12],[y21,y22]]) =
    Mat [[x11+y11,x12+y12],[x21+y21,x22+y22]]
  mul (Mat [[x11,x12],[x21,x22]]) (Mat [[y11,y12],[y21,y22]]) =
    Mat [[z11,z12],[z21,z22]]
    where
      z11 = x11*y11+x12*y21
      z12 = x11*y12+x12*y22
      z21 = x21*y11+x22*y21
      z22 = x21*y12+x22*y22

mat1 :: Mat2x2
mat1 = Mat [[1,2],[3,4]]

mat2 :: Mat2x2
mat2 = Mat [[1,1],[1,1]]

ex3 :: T.Test
ex3 = T.TestList
      [
        U.teq "ex30" (add mat1 mat2)      (Mat [[1+1,2+1],[3+1,4+1]])
--      , U.teq "ex31" (parse "13")        (Just (MkMod 3, ""))
  --    , U.teq "ex32" (parseRing "1+2+5") (Just (MkMod 3))
    --  , U.teq "ex33" (parseRing "1+2*5") (Just (MkMod 1))        
      ]
                       


--------------------------------------------------------------------------------
--- Parser
--------------------------------------------------------------------------------
{-
-- Using stripPrefix function
instance Parsable Bool where
  parse str = case stripPrefix "True" str of
    Just trueRest -> Just (True, trueRest)
    Nothing -> case stripPrefix "False" str of
      Just falseRest -> Just (False, falseRest)
      Nothing -> Nothing
-}

-- using pattern guards
{-
instance Parsable Bool where
  parse str
    | Just rest <- stripPrefix "True" str = Just (True, rest)
    | Just rest <- stripPrefix "False" str = Just (False, rest)
    | otherwise = Nothing
-}

-- should work, but does not
instance Parsable Bool where
  parse = listToMaybe . reads

hw5 :: IO T.Counts
hw5 = do
  T.runTestTT parseTests
  T.runTestTT ex2
  T.runTestTT ex3  
  
