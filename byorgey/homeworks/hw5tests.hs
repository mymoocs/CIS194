{-# OPTIONS_GHC -Wall  #-}


import Test.Hspec
import ExprT
import Parser
import Hw5

main :: IO ()
main = hspec $ do
  describe "Homework 5 tests" $ do
    it "eval function" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20
    it "parseExp function" $ do
      parseExp Lit Add Mul "(2+3)*4"
        `shouldBe` Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    it "evalStr function" $ do
      evalStr "(2+3)*4"  `shouldBe` Just 20
   -- it "Expr type class" $ do
    -- (mul (add (lit 2) (lit 3)) (lit 4)) :: ExprT
      --  `shouldBe` 20  -- ( Mul (Add (Lit 2) (Lit 3)) (Lit 4))
