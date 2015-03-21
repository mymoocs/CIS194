{-# Options_GHC -Wall #-}

module Hw04 where
import Hw04_BST
import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

import Data.Char(isUpper) 

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
{-
data BST a = Leaf
           | Node (BST a) a (BST a)
  deriving Show
-}

compInt :: Int -> Int -> Ordering
compInt x y |x > y = GT
            |x < y = LT
            |True  = EQ
                     
empty :: BST a
empty = Leaf

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ a Leaf = Node Leaf a Leaf
insertBST f k ts@(Node l a r) = case f k a of
                              GT -> Node l a (insertBST f k r)
                              LT -> Node (insertBST f k l) a r
                              _  -> ts

tree :: BST Int
tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 5 Leaf)
ex13 :: T.Test
ex13 = T.TestList
       [
         U.teq "ex130" (insertBST compInt 2 empty) (Node Leaf 2 Leaf)
       , U.teq "ex131" (insertBST compInt 8 tree ) (Node (Node Leaf 1 Leaf) 2 (Node Leaf 5 (Node Leaf 8 Leaf)))
       ]


--------------------------------------------------------------------------------
--- Exercise 14.
-- |
-- Check to see if a list of strings contains only capitalized words
-- >>> allCaps ["Hi","There"]
-- True
-- >>>allCaps []
-- True
-- >>>allCaps ["", "Blah"]
-- False
-- >>>allCaps ["Hi","there"]
-- False


-- safe helpeprs
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs


allCaps :: [String] -> Bool
allCaps [] = True
allCaps ys= all (\a->a == True) $ boolList ys
  where
    boolList [] = [True]
    boolList (x:xs) = case safeHead x of
                       Nothing -> [False]
                       Just c  -> isUpper c : boolList xs

ex14 :: T.Test
ex14 = T.TestList
       [
         U.teq "ex140" (allCaps ["Hi","There"]) True
       , U.teq "ex141" (allCaps []) True
       , U.teq "ex142" (allCaps ["", "Blah"]) False
       , U.teq "ex143" (allCaps ["Hi","there"]) False
       ]

--------------------------------------------------------------------------------
--- Exercise 15.
-- |
-- Drop the trailing whitespace from a string:


dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = filter (/= ' ')

ex15 :: T.Test
ex15 = T.TestList
  [
    U.teq "ex150" (dropTrailingWhitespace "foo") "foo"
  , U.teq "ex151" (dropTrailingWhitespace "") ""
  , U.teq "ex152" (dropTrailingWhitespace "bar ") "bar"
  ]


--------------------------------------------------------------------------------
--- Exercise 16.

-- |
-- Get the first letter (if it exists) of a list of strings:
-- >>>firstLetters ["foo", "bar"]
--['f','b']
-- >>>firstLetters ["alpha",""]
--['a']
-- >>>firstLetters []
--[]
-- >>>firstLetters ["",""]
--[]
firstLetters :: [String] -> [Char]
firstLetters = map (\(x:_)->x)

ex16 :: T.Test
ex16 = T.TestList
       [
         U.teq "160" (firstLetters ["foo", "bar"]) ['f','b']
         , U.teq "161" (firstLetters ["alpha",""]) ['a']
         , U.teq "162" (firstLetters []) []
         , U.teq "163" (firstLetters ["",""]) []
       ]
--------------------------------------------------------------------------------
--- Exercise 17.
-- |
-- Render a proper bracketed list given a list of strings.
-- >>>asList ["alpha","beta","gamma"]
--"[alpha,beta,gamma]"
-- >>>asList []
--"[]"
-- >>>asList ["lonely"]
--"[lonely]"

asList :: [String] -> String
asList [] = "]"
asList (x:xs) = ","++x++ asList xs -- TODO fix,

ex17 :: T.Test
ex17 = T.TestList
       [
         U.teq "ex170" (asList ["alpha","beta","gamma"]) "[alpha,beta,gamma]"
       , U.teq "ex171" (asList []) "[]"
       , U.teq "ex172" (asList ["lonely"]) "[lonely]"
       ]

hw4 :: IO T.Counts
hw4 = do
  T.runTestTT ex13
  T.runTestTT ex14
  T.runTestTT ex15
  T.runTestTT ex16
  T.runTestTT ex17
  
