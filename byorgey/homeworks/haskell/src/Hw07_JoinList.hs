{-# OPTIONS_GHC -Wall #-}

-- these are for exercise 4
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hw07_JoinList where

import           Hw07_Sized
import           Hw07_Scrabble
import           Hw07_Buffer
import           Data.Monoid

import qualified Test.HUnit as T
import qualified Test.HUnit.Util  as U

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)
           
jlProduct :: JoinList (Product Int) Char
jlProduct = Append (Product 210)
            (Append (Product 30)
             (Single (Product 5) 'y')
             (Append (Product 6)
              (Single (Product 2) 'e')
              (Single (Product 3) 'a')))
                 (Single (Product 7) 'h')

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2                       

--1 .
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty  jls = jls
(+++) jls Empty = jls
(+++) js1 js2  = Append (tag js1 `mappend` tag js2) js1 js2 

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Ex 2.
-- safe list indexing function
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

-- 2.
-- data structure for testing find from internet

{-
Append (Size 4)
       (Append (Size 2)
               (Single (Size 1) 'y')  -- 0
               (Single (Size 1) 'e')) -- 1
       (Append (Size 2)
               (Single (Size 1) 'a')  -- 2
               (Single (Size 1) 'h')) -- 3
-}
jlSize :: JoinList Size Char
jlSize = Append (Size 9)
                (Append (Size 5)
                        (Append (Size 2)
                                (Single (Size 1) 'a')
                                (Single (Size 1) 'b'))
                        (Append (Size 3)
                                (Append (Size 2)
                                        (Single (Size 1) 'c')
                                        (Single (Size 1) 'd'))
                                (Single (Size 1) 'e')))
              (Append (Size 4)
                      (Append (Size 2)
                              (Single (Size 1) 'f')
                              (Single (Size 1) 'g'))
                      (Append (Size 2)
                              (Single (Size 1) 'h')
                              (Single (Size 1) 'i')))



ex2_1 :: T.Test
ex2_1 = T.TestList
              [
                U.teq  "compare with `!!?`" (indexJ 2 jlSize)  (jlToList jlSize !!? 2)
              ]


-- indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
-- return (Just element) at the specified index,
-- if index is  out of bouds, the function returns Nothing.
-- indexJ i list =  | stub

indexJ _ Empty = Nothing
indexJ _ (Single _ x) = Just x

indexJ n (Append (Size v) l r) 
    | n >= v = Nothing
    | n < lsize = indexJ n l
    | otherwise =  indexJ (n - lsize) r
    where 
      lsize = getSize (tag l)

-- 2.2
-- drops the first n elements from the list
ex2_2 :: T.Test
ex2_2 = T.TestList
        [
          U.teq "comapre with drop"
                (jlToList (dropJ 3 jlSize))  (drop 3 (jlToList jlSize))
        ]

--dropJ :: (Eq a, Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 s@(Single _ _) = s
dropJ _ (Single _ _) = Empty
dropJ n (Append (Size v) l r) 
    | n >= v = Empty
    | n < lsize = Append (Size (v-n)) (dropJ n l) r
    | otherwise = Append (Size (v-n)) Empty (dropJ (n - lsize) r)
    where 
      lsize = getSize (tag l)


-- 2.3
ex2_3 :: T.Test
ex2_3 = T.TestList
        [
          U.teq "compare with take"
                (jlToList (takeJ 3 jlSize))  (take 3 (jlToList jlSize))
        ]
-- takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ _ s@(Single _ _) = Empty
takeJ n jl@(Append (Size v) l r) 
    | n >= v = jl 
    | n < lsize = Append (Size n) (takeJ n l) Empty 
    | otherwise = Append (Size n) l (takeJ (n - lsize) r)
    where 
      lsize = getSize (tag l)


-- 3. see Hw07_Scrabble
--

ex3 :: T.Test
ex3 = T.TestList
      [
        U.teq "e31" (score 'a') (Score 1)
      , U.teq "e32" (score 'z') (Score 10)
      , U.teq "e33" (scoreString "Arthur") (Score 1+1+1+4+1+1)
      , U.teq "e30"
              (scoreLine "yay " +++ scoreLine "haskell!")
              (Append (Score 23)
               (Single (Score 9) "yay ")
               (Single (Score 14) "haskell!"))
      ]

scoreLine :: String -> JoinList Score String
scoreLine "" = Empty
scoreLine str = Single  (scoreString str) str

-- 4.

instance Buffer (JoinList (Score, Size) String) where
  toString   = concat . jlToList
  fromString "" = Empty
  fromString s@(c:cs) = Append (score c, Size (length s))
                             (Single (score c, Size 1) [c])
                             (fromString cs)
  
  


hw07 :: IO T.Counts
hw07 = do
    _ <- T.runTestTT ex2_1
    _ <- T.runTestTT ex2_2
    _ <- T.runTestTT ex2_3
    T.runTestTT ex3


-- End of file
