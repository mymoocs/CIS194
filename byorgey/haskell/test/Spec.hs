import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Cis194

main :: IO ()
main = hspec $ do
    describe "Hw01" $ do
      it "convert positive Integer to a list of digits" $ do
        toDigits 123 `shouldBe` [1,2,3]
      it "returns the [] input 0" $
        toDigits 0     `shouldBe` []
      it "returns the [] if not positive input" $
        toDigits (-14) `shouldBe` []
      it "convert positive Integer to a reversed list of digits" $ do
        toDigitsRev 123 `shouldBe` [3,2,1]
      it "should double every other number beginning from the right" $ do
        doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
      it "should double every other number beginning from the right" $ do
        doubleEveryOther [1,2,3] `shouldBe` [1,4,3]
        
    describe "Hw02" $ do
      it "convert positive Integer to a reversed list of digits" $ do
        toDigitsRev 123 `shouldBe` [3,2,1]
    
