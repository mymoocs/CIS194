{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hw12_Risk where

import           Control.Monad.Random

import qualified Test.HUnit               as T
import qualified Test.HUnit.Util          as U

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }



--------------------------------------------------------------------------------
-- 1.

ex1 :: T.Test
ex1 = T.TestList
    [
    ]

threeInts :: Rand StdGen (Int, Int, Int)
threeInts =
  getRandom >>= \i1 ->
  getRandom >>= \i2 ->
  getRandom >>= \i3 ->
  return (i1,i2,i3)

--------------------------------------------------------------------------------
-- 2.

battle :: Battlefield -> Rand StdGen Battlefield
battle = undefined

ex2 :: T.Test
ex2 = T.TestList
      [
      ]
--------------------------------------------------------------------------------
-- 3.

ex3 :: T.Test
ex3 = T.TestList
      [
      ]
      
--------------------------------------------------------------------------------
-- 4.

ex4 :: T.Test
ex4 = T.TestList
      [
      ]

--------------------------------------------------------------------------------
-- 5.

ex5 :: T.Test
ex5 = T.TestList
      [
      ]



--------------------------------------------------------------------------------
hw12 :: IO T.Counts
hw12 = do
  T.runTestTT ex1
  T.runTestTT ex2
  T.runTestTT ex3
  T.runTestTT ex4
  T.runTestTT ex5
