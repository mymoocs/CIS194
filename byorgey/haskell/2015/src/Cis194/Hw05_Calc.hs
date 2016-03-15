{-# LANGUAGE TypeSynonymInstances #-} -- need for ex 5.
{-# LANGUAGE FlexibleInstances #-}    -- need for ex 6.

 module Cis194.Hw05_Calc
    ( eval
    , evalStr
    , VarExprT(..)
    , ExprT(..)
    , Expr(..)
    , HasVars(..)
    , parseExp
    , compile
    , exec
    , withVars)
    where

import Cis194.ExprT
import Cis194.VarExprT
import Cis194.Parser
import qualified Cis194.StackVM  as S
import qualified Data.Map        as M
    
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



------------------------------------------------------------------------------
-- Exercise 3:

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add       
    mul = Mul

------------------------------------------------------------------------------
-- Exercise 4:
          
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit n
        | n <= 0 = False
        | otherwise = True
    add = (||)
    mul = (&&)      


newtype  MinMax = MinMax Integer deriving (Eq, Ord, Show)
instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min


newtype Mod7 = Mod7 Integer deriving (Eq,  Show)
instance Expr Mod7 where
    lit =  fromInteger
    add =  (+)
    mul =  (*)     

instance Num Mod7 where
    Mod7 a + Mod7 b = Mod7 $ mod (a+b) 7
    Mod7 a * Mod7 b = Mod7 $ mod (a*b) 7                      
    abs (Mod7 a)    = Mod7 $ abs a
    signum (Mod7 a) = Mod7 $ signum a
    fromInteger a   = Mod7 $ mod a 7
    negate (Mod7 a) = Mod7 $ negate a



testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

------------------------------------------------------------------------------
-- Exercise 5:

instance Expr [S.StackExp] where
    lit n = [S.PushI n] 
    add xs ys = xs++ys++[S.Add]
    mul xs ys = xs++ys++[S.Mul]      

compile :: String -> Maybe S.Program
compile = parseExp lit add mul


exec :: Maybe S.Program -> Either String S.StackVal
exec p = case  p of
           Just a -> S.stackVM  a
           Nothing -> Left "failure"

                                
    
------------------------------------------------------------------------------
-- Exercise 6:


class HasVars a where
    var :: String -> a


instance HasVars VarExprT where
    var  = VVar

instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul      
    
instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup
            
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n =  \_ -> Just n
    add e1 e2 = \m -> case e1 m of
                        Just x -> case e2 m of
                                    Just y -> Just $ x + y
    mul e1 e2 = \m -> case e1 m of
                        Just x -> case e2 m of
                                    Just y -> Just $ x * y
    

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs e = e $ M.fromList vs
                                                    
