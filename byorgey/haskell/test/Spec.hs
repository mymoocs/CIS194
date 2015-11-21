import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do
    describe "Hw01" $ do
      it "returns the first element of a list" $ do
        head [23 ..] `shouldBe` (23 :: Int)
    
      it "returns the first element of an *arbitrary* list" $
        property $ \x xs -> head (x:xs) == (x :: Int)
    
      it "throws an exception if used with an empty list" $ do
        evaluate (head []) `shouldThrow` anyException
    describe "Hw02" $ do
      it "returns the first element of a list" $ do
        head [23 ..] `shouldBe` (23 :: Int)
    
      it "returns the first element of an *arbitrary* list" $
        property $ \x xs -> head (x:xs) == (x :: Int)
    
      it "throws an exception if used with an empty list" $ do
        evaluate (head []) `shouldThrow` anyException
    
