{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
-- | Test suite for Homework 01.

module Cis194.Hw01.HSpec where

import SpecHelper

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec = do
  describe "toDigits" testToDigits

-- | Test toDigits.
testToDigits :: Spec
testToDigits =
    do it "convert positive Integer to a list of digits"
          (toDigits 123 `shouldBe` [1,2,3])
       it "returns the [] input 0"
          (toDigits 0     `shouldBe` [])
       it "returns the [] if not positive input"
          (toDigits (-14) `shouldBe` [])


-- | Test toDigitsRev.
testToDigitsRev :: Spec
testToDigitsRev =
    do it "convert positive Integer to a reversed list of digits"
          (toDigitsRev 123 `shouldBe` [3,2,1])

-- | Test doubleEveryOther.
testDoubleEveryOther :: Spec
testDoubleEveryOther =
    do it "should double every other number beginning from the right"
          (doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5])
       it "should double every other number beginning from the right"
          (doubleEveryOther [1,2,3] `shouldBe` [1,4,3])
