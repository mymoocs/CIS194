module Cis194.Hw03Spec (spec) where

import           Cis194.Hw03_Golf
import           Test.Hspec

spec :: Spec
spec = do
  describe "skips" $ do
    it "input 'ABCD'" $ do
      skips "ABCD" `shouldBe` ["ABCD","BD","C","D"]
    it "input 'hello!'" $ do
      skips "hello!" `shouldBe` ["hello!","el!","l!","l","o","!"]
    it "input [1]" $ do
      skips [1] `shouldBe` ([[1]]::[[Int]])
    it "input bool list" $ do
      skips [True,False] `shouldBe` [[True,False],[False]]
    it "input []" $ do
      skips [] `shouldBe` ([]::[[Int]])
  describe "histogram" $ do
    it "[3,5]" $ do
      histogram [3,5] `shouldBe` "   * *    \n==========\n0123456789\n"

                
