{-# OPTIONS_GHC -Wall #-}

module Hw5 where

import ExprT
import Parser

-- 1.

-- eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.
eval :: ExprT -> Integer
eval e = case e of
          (Lit n) -> n
          (Add e1 e2) -> eval e1 + eval e2
          (Mul e1 e2) -> eval e1 * eval e2


-- 2.

evalStr :: String -> Maybe Integer
evalStr str = let res = parseExp Lit Add Mul str
              in case res of
                  Nothing -> Nothing
                  Just e -> Just $ eval e


-- 3.

reify :: ExprT -> ExprT
reify = id
