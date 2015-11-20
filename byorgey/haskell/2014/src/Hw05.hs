{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Hw05 where

import ExprT
import Parser
import qualified StackVM as S -- need for exercise 5

import qualified Data.Map as M
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
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a


instance Expr ExprT where
  lit n = Lit n
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2
  
-- To the untrained eye it may look like reify does no actual work!
-- But its real purpose is to constrain the type of its argument to ExprT.  
reify :: ExprT -> ExprT
reify = id

-- 4.

instance Expr Integer where
  lit n = n
  add e1 e2 = e1 + e2
  mul e1 e2 = e1 * e2

instance Expr Bool where
  lit n = if n <= 0 then True else False
  add e1 e2 = e1 || e2
  mul e1 e2 = e1 && e2

newtype MinMax = MinMax Integer
               deriving (Eq, Show)
newtype Mod7 = Mod7 Integer
             deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax n) (MinMax m) = if n > m then MinMax n else MinMax m
  mul (MinMax n) (MinMax m) = if n < m then MinMax n else MinMax m

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 n) (Mod7 m) = Mod7 (mod (n+m) 7)
  mul (Mod7 n) (Mod7 m) = Mod7 (mod (n*m) 7)

-- It’s great how easy it is for us to swap in new semantics for
-- the same syntactic expression!
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

  
-- 5.

instance Expr S.Program where
  lit n = [S.PushI n]
  add e1 e2 = e1++e2++[S.Add]
  mul e1 e2 = e1++e2++[S.Mul]

testProgram :: Maybe S.Program
testProgram = testExp :: Maybe S.Program  

calculate :: [Char] -> Either [Char] S.StackVal                         
calculate str  = let stack = compile str
                 in case stack of
                     Nothing -> Left "Nothing"
                     Just s ->  S.stackVM s

compile :: String -> Maybe S.Program
compile  =  parseExp lit add mul

-- 6. arithmetic expressions the with variables.

class HasVars a where
  var :: String -> a

instance HasVars VarExprT where  
  var s = VVar s

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var  = M.lookup   

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n =  \_ -> Just n

  add e1 e2 = \m -> case e1 m of
                     Just x -> case e2 m of
                                Just y -> Just $ x + y
  mul e1 e2 = \m -> case e1 m of
                     Just x -> case e2 m of
                                Just y -> Just $ x * y
{--
with Monads
add e1 e2 = \m -> e1 m >>= \x -> e2 m >>= \y -> return (x+y)


mul e1 e2 = \m -> do
    x <- e1 m
    y <- e2 m
    return (x * y) --}
    
  

instance Expr VarExprT where
  lit  = VLit 
  add  = VAdd 
  mul  = VMul
  
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs e = e $ M.fromList vs

