module Cis194.Hw02Spec (spec) where

import           Cis194
import           Test.Hspec

spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "info log" $ do
      parseMessage "I 29 la la la" `shouldBe`  LogMessage Info 29 "la la la"

