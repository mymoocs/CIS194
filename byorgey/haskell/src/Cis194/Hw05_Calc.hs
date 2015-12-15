module Cis194.Hw05_Calc
    (eval
    , evalStr
    , ExprT(..)
    , parseExp)
    where

import Cis194.ExprT
import Cis194.Parser

------------------------------------------------------------------------------
-- Exercise 1:
-- |
--
-- >>> eval (Mul (Add  (Lit 2) (Lit 3)) (Lit 4))
-- 20

eval :: ExprT -> Integer
eval e = case e of
           Lit n     -> n
           Add e1 e2 -> (+) (eval e1)  (eval e2)
           Mul e1 e2 -> (*) (eval e1)  (eval e2)

------------------------------------------------------------------------------
-- Exercise 2:


evalStr :: String -> Maybe Integer
evalStr = fmap eval .  parseExp Lit Add Mul
