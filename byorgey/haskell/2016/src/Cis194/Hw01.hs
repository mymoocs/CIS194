-- CIS 194 Homework 1

module Cis194.Hw01 where

import Data.Char (digitToInt)
import Data.List (mapAccumR, foldr)
------------------------------------------------------------------------------
-- Exercise 1

-- | @toDigits n@ convert positive Integers to a list of digits.
--
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigits 0
-- []
--
-- >>> toDigits (-17)
-- []
toDigits :: Integer -> [Integer]
toDigits n
         | n <= 0 = []
         | otherwise = map (toInteger . digitToInt) $ show n


-- | @toDigitsRev n@ do the same as @toDigits n@, but with the digits reversed.
-- >>> toDigitsRev 1234
-- [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev  = reverse . toDigits



-- | toDigits' should convert positive Integers to a list of digits.
--  using mutula recursion
--
-- >>> toDigits 1234
-- [1,2,3,4]
--
-- >>> toDigits 0
-- []
--
-- >>> toDigits (-17)
-- []

toDigits' :: Integer -> [Integer]
toDigits' n
    | n <= 0 = []
    | otherwise = toDigitsExceptLast' n ++ lastDigit n


lastDigit :: Integer -> [Integer]
lastDigit = (: []) . (`mod` 10)

-- | point free syntax
toDigitsExceptLast' :: Integer -> [Integer]
toDigitsExceptLast'  = toDigits' . flip div 10

toDigitsExceptLast'' :: Integer -> [Integer]
toDigitsExceptLast'' n = toDigits' m where m = div n 10


-------------------------------------------------------------------------------
-- Exercise 2


-- | should double every other number beginning from the right, that is,
--   the second-to-last, fourth-to-last,. . . numbers are doubled.
--
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
--
-- >>> doubleEveryOther [1,2,3]
--[1,4,3]



doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
    | even n =  doubleOdd xs
    | otherwise = doubleEven xs
    where n = length xs


-- | use mutual recusion to double even positioned elements in the list.
doubleEven :: Num a => [a] -> [a]
doubleEven (x:xs) = x : doubleOdd xs
doubleEven [] = []

-- | use mutual recursion to double odd positioned elements in the list.
doubleOdd :: Num a => [a] -> [a]
doubleOdd (x:xs) = (2*x) : doubleEven xs
doubleOdd [] = []

-- | other nice solutions, found in internet
-- https://stackoverflow.com/questions/23842473/what-to-use-instead-of-explicit-recursion-in-haskell
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' = reverse . zipWith (*) (cycle [1,2]) . reverse

-- | mapAccumR function from Data.List
doubleEveryOther'' :: Num a => [a] -> [a]
doubleEveryOther'' = snd . mapAccumR step False
  where
    step False x = (True, x)
    step True  x = (False, 2*x)


-- | can use foldr to do this kind of recursion from the right:
doubleEveryOther''' :: Num a => [a] -> [a]
doubleEveryOther''' = snd . foldr go (False, [])
    where go x (b, xs) = (not b, (if b then 2*x else x) : xs)



------------------------------------------------------------------------------
-- Exercise 3

-- | to calculate the sum of all digits of given list.
--
-- >>> sumDigits [16,7,12,5]
-- 22

-- 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits  =  sum . concat .  map toDigits


------------------------------------------------------------------------------
-- Exercise 4


-- | credit card validation algorithm
--
-- >>> validate 4012888888881881
-- True
--
-- >>> validate 4012888888881882
-- False

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
