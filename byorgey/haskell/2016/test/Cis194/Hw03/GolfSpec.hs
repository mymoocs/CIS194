module Cis194.Hw03.GolfSpec where

import SpecHelper


-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "function skips"  testSkips



testSkips :: Spec
testSkips = do
  it "simple case" (skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"])
