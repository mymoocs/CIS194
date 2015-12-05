module Cis194.Hw03_Golf
    (skips
    , localMaxima 
    , histogram
    )
    where

import Data.List       (sort, group)
------------------------------------------------------------------------------
-- Exercise 1
      
-- | The output of skips is a list of lists. The first list in the output should
--   be the same as the input list. The second list in the output should
--   contain every second element from the input list. . . and the nth list in
--   the output should contain every nth element from the input list.
--
-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
-- >>> skips [1]
-- [[1]]
-- >>> skips [True,False]
-- [[True,False],[False]]
-- >>> skips []
-- []

skips :: [a] -> [[a]]
skips [] = []
skips xs  = skip 0  xs
    where
      n = length xs
      skip k xs
          | k >= n = []
          | otherwise = everyNth k xs :  skip (k+1) xs 
                      

everyNth _ [] = []            
everyNth 0 xs  = xs                  
everyNth n xs = if null ds then [] else head ds : everyNth n (drop (n+1) xs)
    where
      ds = drop n xs

everyNth1 :: Int -> [a] -> [a]
everyNth1 0 xs = xs
everyNth1 n xs =
    let
        ds = drop n xs
    in
      if null ds
      then []
      else  head ds : everyNth1 n (drop (n+1) xs)

------------------------------------------------------------------------------
-- Exercise 2

-- | A local maximum of a list is an element of the list which is strictly
--   greater than both the elements immediately before and after it.
--
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
-- >>> localMaxima [2,3,4,1,5]
-- [4]
-- >>> localMaxima [1,2,3,4,5]
-- []
localMaxima :: [Integer] -> [Integer]
localMaxima []  = []
localMaxima [z] = []                 
localMaxima all@(x:y:xs) =  localMax xs (y:xs) all
    where
      localMax [] _ _ = []
      localMax (x:xs) (y:ys) (z:zs)
          | x < y && z < y  = y:localMax xs ys zs
          | otherwise       = localMax xs ys zs         
    
                            

------------------------------------------------------------------------------
-- Exercise 3

-- | which takes as input a list of Integers between 0 and 9 (inclusive),
--   and outputs a vertical histogram showing how many of each number
-- were in the input list.
{- 
histogram [1,1,1,5] ==
          *
          *
          * *
          ==========
         0123456789
-}
-- >>> 
histogram :: [Integer] -> String
histogram = (++"==========\n0123456789\n") .  histo . ratio . group . sort

histo :: [[Integer]] -> String
histo = concatMap (histo' vs) . reverse
    where
      vs :: [Integer]
      vs = [0,1,2,3,4,5,6,7,8,9]
      histo' _ []       = []     
      histo' [] _       = "\n"
      histo' (y:ys) xs
          | y `elem` xs = '*' : histo' ys xs
          | otherwise   = ' ' :  histo' ys xs

ratio :: [[a]] -> [[a]]
ratio []  = []
ratio xss = concatMap pickFirst xss : ratio (filter notnull (map dropFirst xss))
    where
      dropFirst []     = []
      dropFirst (x:xs) = xs
      pickFirst []     = []
      pickFirst (x:xs) = [x]
      notnull :: [a] -> Bool                   
      notnull = (not . null)
