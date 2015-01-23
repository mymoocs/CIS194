{-# OPTIONS_GHC -Wall #-}
module Hw07_JoinList where

import Hw07_Sized
import Data.Monoid

import qualified Test.HUnit as T

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

-- 2.1
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



-- test_indexJ :: T.Test
test_indexJ :: IO T.Counts
test_indexJ = T.runTestTT $ 
              T.TestList
                   [
                    T.TestCase $ T.assertEqual "equal with !!?"
                         (indexJ 2 jlSize)  ( (jlToList jlSize !!? 2))
                   ]
-- indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
-- return (Just element) at the specified index,
-- if index is  out of bouds, the function returns Nothing.
-- indexJ i list =  | stub

indexJ _ Empty = Nothing
indexJ _ (Single 1 x) = Just x
indexJ n (Append (Size v) l r) 
    | n >= v = Nothing
    | n < lsize = indexJ n l
    | otherwise =  indexJ (n - lsize) r
    where 
      lsize = getSize (tag l)


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl = undefined


