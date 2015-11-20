{-# OPTIONS_GHC -Wall  #-}


import Test.Hspec
import ExprT
import Parser
import Hw5
import qualified StackVM as S -- need for exercise 5

main :: IO ()
main = hspec $ do
  describe "Homework 5 tests" $ do
    it "Ex 1. eval function" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
    it "Ex 2. parseExp function" $ do
      parseExp Lit Add Mul "(2+3)*4"
        `shouldBe` Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    it "Ex 3.  valStr function" $ do
      evalStr "(2+3)*4"  `shouldBe` Just 20
    -- it "Ex 5. Compile string to Program" $ do
      -- calculate "(3 * -4) + 5" `shouldBe` Right (S.IVal (-7))
