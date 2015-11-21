{-# OPTIONS_GHC -Wall #-}
module Hw01
    (
      toDigits
    , toDigitsRev
    , doubleEveryOther
    , sumDigits
    , validate  
    ) where

import Data.Char (digitToInt)

------------------------------------------------------------------------------
-- Tests from https://github.com/haroldcarr/
{--e1 :: T.Test
e1 = T.TestList
    [
      U.teq "123last" (lastDigit     123)   3
    , U.teq "0last"   (lastDigit     0)     0
    , U.teq "123drop" (dropLastDigit 123)  12
    , U.teq "5drop"   (dropLastDigit 5)     0
      ---------------------------------------
    , U.teq "397last" (lastDigit     397)   7
    , U.teq "397drop" (dropLastDigit 397)  39
    , U.teq "39last"  (lastDigit     39)    9
    , U.teq "39drop"  (dropLastDigit 39)    3
    , U.teq "3last"   (lastDigit     3)     3
    , U.teq "3drop"   (dropLastDigit 3)     0
    ]
--}

------------------------------------------------------------------------------
-- Exercise 1

-- | toDigits should convert positive Integers to a list of digits. (For 0 or
-- negative inputs, toDigits should return the empty list.)
--
-- >>> toDigits 1234
-- [1,2,3,4]
--
-- >>> toDigits 0
-- []
--
-- >>> toDigits (-17)
-- []
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = map (toInteger . digitToInt) $ show n


-- | should do the same as toDigits, but with the digits reversed
-- >>> toDigitsRev 1234
-- [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits


------------------------------------------------------------------------------
-- Exercise 2

-- | should double every other number beginning from the right, that is,
--   the second-to-last, fourth-to-last,. . . numbers are doubled.
--
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
--
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ys
  | n `mod` 2 == 0 = doubleFst ys
  | otherwise  = doubleSnd ys
    where n = length ys
          doubleFst [] = []
          doubleFst (x:xs) = 2*x : doubleSnd xs
          doubleSnd [] = []
          doubleSnd (x:xs) = x:doubleFst xs
          


------------------------------------------------------------------------------
-- Exercise 3

-- | to calculate the sum of all digits.
--
-- >>> let r = 1 + 6 + 7 + 1 + 2 + 5          
-- >>> sumDigits [16,7,12,5]
-- 22 
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits


------------------------------------------------------------------------------
-- Exercise 4

-- | indicates whether an Integer could be a valid credit card number.
-- >>> validate 4012888888881881
-- True
--
-- >>> validate 4012888888881882
-- False
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
