module Cis194.Hw01Spec (spec) where

import           Cis194.Hw01
import           Test.Hspec

spec :: Spec
spec = do
  describe "toDigits" $ do
    it "convert positive Integer to a list of digits" $ do
      toDigits 123 `shouldBe` [1,2,3]
    it "returns the [] input 0" $
      toDigits 0     `shouldBe` []
    it "returns the [] if not positive input" $
      toDigits (-14) `shouldBe` []
  describe "toDigitsRev" $ do      
    it "convert positive Integer to a reversed list of digits" $ do
      toDigitsRev 123 `shouldBe` [3,2,1]
  describe "doubleEveryOther" $ do    
    it "should double every other number beginning from the right" $ do
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
    it "should double every other number beginning from the right" $ do
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

