{-# Options_GHC -Wall #-}

module Hw04 where
import Hw04_BST
--------------------------------------------------------------------------------
--- Exercise 1.
-- |
-- 1 possible function of this type

ex1 :: a -> b -> b
ex1 _ b = b

--------------------------------------------------------------------------------
--- Exercise 2.
-- |
-- 2 function of this type,
-- function that returns its frist argument
-- function that returns its second argument
ex2 :: a -> a -> a
ex2 x _ = x

--------------------------------------------------------------------------------
--- Exercise 3.
-- |
-- 1 possible answer is.
-- We know that this must be so because example1 returns an a.
-- We don’t know what a is. And, example1 takes in exactly one a.
-- So, the only possible valid return value is the one and only a
-- we were passed in! 

ex3 :: Int -> a -> a
ex3 _ x = x


--------------------------------------------------------------------------------
--- Exercise 2.
-- |
-- 2 possible functions
-- return first a type argument
-- return second a type argument

ex4 :: Bool -> a -> a -> a
ex4 b x y |b         = x
          |otherwise = y

--------------------------------------------------------------------------------
--- Exercise 5.

ex5 :: Bool -> Bool
ex5 b = b && b

--------------------------------------------------------------------------------
--- Exercise 6.
-- |
-- 2 possible functions
-- return f apply to a
-- return a
ex6 :: (a -> a) -> a
ex6 = error "impossible"

--------------------------------------------------------------------------------
--- Exercise 7.
-- |
-- 2 possible functions
-- return f apply to a
-- return a

ex7 :: (a -> a) -> a -> a
ex7 f a = f a

--------------------------------------------------------------------------------
--- Exercise 8.
-- |
-- infinite set of functions
ex8 :: [a] -> [a]
ex8 [] = []
ex8  (x:_) = [x]


--------------------------------------------------------------------------------
--- Exercise 9.
-- |
-- infinite set of functions

ex9 :: (a -> b) -> [a] -> [b]
ex9 _ [] = []
ex9 f (x:_) = [f x] 

--------------------------------------------------------------------------------
--- Exercise 10.
-- |
-- ex10 Nothing = is impossible case
ex10 :: Maybe a -> a
ex10 = error "impossible"

--------------------------------------------------------------------------------
--- Exercise 11.
-- |
-- 1 possible function
ex11 :: a -> Maybe a
ex11 = Just


--------------------------------------------------------------------------------
--- Exercise 12.
-- |
-- 1 possible function
-- We don’t know what a is. And, ex12 takes in exactly one Maybe a.
-- we could not do any operation on a, due to unknown type.
ex12 :: Maybe a -> Maybe a
ex12 a = a

-- Binary search trees


--------------------------------------------------------------------------------
--- Exercise 13.
-- |
-- the insertion method for a binary search tree:
-- >>> 

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST = undefined

