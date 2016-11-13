{-# LANGUAGE FlexibleInstances #-}

module Cis194.Hw05.Calc
    where

import Cis194.Hw05.ExprT
import Cis194.Hw05.Parser
import qualified Cis194.Hw05.StackVM as VM
import qualified Data.Map as M

-------------------------------------------------------------------------------
-- Exercise 1

-- |
-- >>> eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
-- 20
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2


-- |
-- >>> parseExp Lit Add Mul "(2+3)*4"
-- Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
--
-- >>> parseExp Lit Add Mul "2+3*4"
-- Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
--
-- >>> parseExp Lit Add Mul "2+3*"
-- Nothing
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul


-------------------------------------------------------------------------------
-- Exercise 3

-- |
-- >>> mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)
--
-- >>> reify $ mul (add (lit 2) (lit 3)) (lit 4)
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id



-------------------------------------------------------------------------------
-- Exercise 4

-- Make instances of Expr for each of the following types:

-- Integer

instance Expr Integer where
    lit n = n
    add = (+)
    mul = (*)


-- Bool
instance Expr Bool where
    lit n
        | n <= 0 = False
        | otherwise = True
    add = (||)
    mul = (&&)

-- MinMax
data MinMax a = MinMax a deriving (Eq, Show, Ord)

instance Expr (MinMax Integer) where
    lit = MinMax
    add = max -- (MinMax n) (MinMax m) = MinMax $ max n m
    mul = min -- (MinMax n) (MinMax m) = MinMax $ min n m

instance Functor MinMax where
--  fmap :: (a -> b) -> f a -> f b
  fmap f (MinMax n) = MinMax (f n)
--   (<$) :: a -> f b -> f a
  (<$) a _ = MinMax a


-- Mod7
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit = Mod7
    add = (+) -- (Mod7 n) (Mod7 m) = Mod7 $ mod (n + m) 7
    mul (Mod7 n) (Mod7 m) = Mod7 $ mod (n * m) 7

instance Num Mod7 where
    Mod7 n + Mod7 m = Mod7 $ mod (n + m) 7


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe (MinMax Integer)
testSat = testExp :: Maybe Mod7


-------------------------------------------------------------------------------
-- Exercise 5

instance Expr VM.Program where
    lit n = [VM.PushI n]
    add n m  = concat [n, m, [VM.Add]]
    mul n m  = concat [n, m, [VM.Mul]]


compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

testVM :: Maybe (Either String VM.StackVal)
testVM = fmap VM.stackVM $ compile "2+3*10+1*4"


-------------------------------------------------------------------------------
-- Exercise 6
data VarExprT = VLit Integer
              | Var String
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
  deriving (Show, Eq)


class HasVars a where
    var :: String -> a


instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul

instance HasVars VarExprT where
    var = Var

-- this instance mean that variables can be interpreted as functions
-- from a mapping of variables to Integer values to (possibly)
-- Integer values. It should work by looking up the variable in the mapping.
instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

-- this  instance says that these same functions can be interpreted
-- as expressions (by passing along the mapping to subexpressions
-- and combining results appropriately)
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n = \_ -> Just n
    add e1 e2 = \m -> (+) <$>  (e1 m) <*> (e2 m)
    mul e1 e2 = \m -> (*) <$>  (e1 m) <*> (e2 m)


-- |
--
-- >>>:t add (lit 3) (var "x")
-- add (lit 3) (var "x") :: (Expr a, HasVars a) => a
-- >>> withVars [("x", 6)] $ add (lit 3) (var "x")
-- Just 9
-- >>> withVars [("x", 6)] $ add (lit 3) (var "y")
-- Nothing
-- >>> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- Just 54
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

reifyV :: VarExprT -> VarExprT
reifyV = id
