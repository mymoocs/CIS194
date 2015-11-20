{-# OPTIONS_GHC -Wall #-}

module Hw01
    (
    ) where


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

toDigits :: Integer -> [Integer]
toDigits = undefined

toDigitsRev :: Integer -> [Integer]
toDigitsRev = undefined




