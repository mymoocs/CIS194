{-# OPTIONS_GHC -Wall #-}

-- these are for exercise 4
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hw07_JoinList where

import           Hw07_Sized
import           Hw07_Scrabble
import           Hw07_Buffer
import           Data.Monoid
import           Data.Char        (toUpper)
import qualified Test.HUnit       as T
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



-- indexJ1 :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
-- return (Just element) at the specified index,
-- if index is  out of bouds, the function returns Nothing.
-- indexJ1 i list =  | stub

indexJ1 _ Empty = Nothing
indexJ1 _ (Single _ x) = Just x

indexJ1 n (Append (Size v) l r) 
    | n >= v = Nothing
    | n < lsize = indexJ1 n l
    | otherwise =  indexJ1 (n - lsize) r
    where 
      lsize = getSize (tag l)

-- my indexJ1 function works only for Size it was not polymorphic on b
-- the resong was that i hard coded dagtaconstructor in
-- (Append (Size v) l r)

-- I found how to skip this limitation at Harold Carr github
-- he used helper function to get size from monad b

gst :: (Sized b, Monoid b) => JoinList b a -> Int
gst = getSize . size . tag

indexJ _ Empty = Nothing
indexJ _ (Single _ x) = Just x

indexJ n j@(Append _ l r) 
    | n >= (gst j) = Nothing
    | n < lsize = indexJ n l
    | otherwise =  indexJ (n - lsize) r
    where 
      lsize = gst l


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
  toString   = init . unlines . jlToList -- init remove extra '\n' at the end
  fromString "" = Empty
  fromString str  = fromString' (lines str)
    where
      fromString' [] = Empty
      fromString' (c:cs) = (Single (scoreString c, Size 1) c)
                           +++ fromString' cs
  line = indexJ
  replaceLine n newLine = rLine n
    where
      rLine _ (Single m oldLine) = Single m newLine
      rLine _ Empty = Empty
      rLine n j@(Append _ l r) 
        | n >= (gst j) = j
        | n < lsize = rLine n l +++ r
        | otherwise = l +++ rLine (n - lsize) r
        where 
          lsize = gst l

  numLines = gst
  value    = getScore  . fst . tag
  

-- for test
type TJL = JoinList (Score, Size) String

cc :: String
cc = "Title: A Christmas Carol"

tu :: String -> String
tu = map toUpper

ex4 :: T.Test
ex4 = T.TestList
    [
      U.teq "e40" ((fromString exSt)::TJL) exJl
    , U.teq "e41" (toString exJl) exSt
    , U.teq "e42" (line 0 exJl) (Just "The Project Gutenberg EBook of A Christmas Carol, by Charles Dickens")
    , U.teq "e42" (line 13 exJl) (Just "Release Date: August 11, 2004 [EBook #46]")
    , U.teq "e43" (line 8 exJl) (Just cc)
    , U.teq "e44" (line 8 (replaceLine 8 (tu cc) exJl)) (Just (tu cc))
    , U.teq "e45" (numLines exJl) 14
    ]

exSt :: String
exSt =
    "The Project Gutenberg EBook of A Christmas Carol, by Charles Dickens\n\
\\n\
\This eBook is for the use of anyone anywhere at no cost and with\n\
\almost no restrictions whatsoever.  You may copy it, give it away or\n\
\re-use it under the terms of the Project Gutenberg License included\n\
\with this eBook or online at www.gutenberg.net\n\
\\n\
\\n\
\Title: A Christmas Carol\n\
\       A Ghost Story of Christmas\n\
\\n\
\Author: Charles Dickens\n\
\\n\
\Release Date: August 11, 2004 [EBook #46]"

exJl :: TJL
exJl = Append (Score 572,Size 14) (Single (Score 110,Size 1) "The Project Gutenberg EBook of A Christmas Carol, by Charles Dickens") (Append (Score 462,Size 13) (Single (Score 0,Size 1) "") (Append (Score 462,Size 12) (Single (Score 90,Size 1) "This eBook is for the use of anyone anywhere at no cost and with") (Append (Score 372,Size 11) (Single (Score 84,Size 1) "almost no restrictions whatsoever.  You may copy it, give it away or") (Append (Score 288,Size 10) (Single (Score 89,Size 1) "re-use it under the terms of the Project Gutenberg License included") (Append (Score 199,Size 9) (Single (Score 66,Size 1) "with this eBook or online at www.gutenberg.net") (Append (Score 133,Size 8) (Single (Score 0,Size 1) "") (Append (Score 133,Size 7) (Single (Score 0,Size 1) "") (Append (Score 133,Size 6) (Single (Score 29,Size 1) "Title: A Christmas Carol") (Append (Score 104,Size 5) (Single (Score 39,Size 1) "       A Ghost Story of Christmas") (Append (Score 65,Size 4) (Single (Score 0,Size 1) "") (Append (Score 65,Size 3) (Single (Score 35,Size 1) "Author: Charles Dickens") (Append (Score 30,Size 2) (Single (Score 0,Size 1) "") (Append (Score 30,Size 1) (Single (Score 30,Size 1) "Release Date: August 11, 2004 [EBook #46]") Empty)))))))))))))

------------------------------------------------------------------------------

hw07 :: IO T.Counts
hw07 = do
    _ <- T.runTestTT ex2_1
    _ <- T.runTestTT ex2_2
    _ <- T.runTestTT ex2_3
    _ <- T.runTestTT ex3
    T.runTestTT ex4


-- End of file
