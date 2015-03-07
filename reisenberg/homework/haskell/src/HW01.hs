{-
Name: Arthur Vardanyan
Collaborators: "none"
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
>> "./doctest.exe" -icsr src/HW01.hs 
-}

{-# OPTIONS_GHC -Wall  #-}
 
module Hw01 where         -- We'll learn more about this later

import Data.Char (digitToInt)

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.

--------------------------------------------------------------------------------
--- Exercise 1.

-- | break up a number into its last digit, return last digit
-- >>> lastDigit 123
-- 3
-- >>> lastDigit 0
-- 0
lastDigit :: Integer -> Integer
lastDigit = toInteger . digitToInt . last . show 

-- | break up a number and return number without last digit
-- >>> dropLastDigit 123
-- 12
-- >>> dropLastDigit 1
-- 0
dropLastDigit :: Integer -> Integer
dropLastDigit n | n < 10 = 0
                | otherwise = f n
  where
    f = read . init . show

--------------------------------------------------------------------------------
--- Exercise 2.

-- | break apart a number into its digits.
-- >>> toDigits 123
-- [1,2,3]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []    

toDigits :: Integer -> [Integer]
toDigits n |n <= 0     = []
           |n <  10    = [n]
           |otherwise  = toDigits m ++ d
  where
    d = [lastDigit n] 
    m = dropLastDigit n

--------------------------------------------------------------------------------
--- Exercise 3.

-- | double every other element in the list begining from last
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]    

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs |even n    =  doubleFirst xs
                    |otherwise =  doubleSecond xs
  where
    n = length xs 

doubleFirst :: [Integer] -> [Integer]
doubleFirst []     = []
doubleFirst (x:xs) = (2*x) : doubleSecond xs

doubleSecond :: [Integer] -> [Integer]
doubleSecond []     = []
doubleSecond (x:xs) = x:doubleFirst xs 


--------------------------------------------------------------------------------
--- Exercise 4.
-- | calculate the sum of all digits in the list of integer numbers.
-- >>> sumDigits [16,7,12,5] == 1 + 6 + 7 + 1 + 2 + 5
-- True

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits


--------------------------------------------------------------------------------
--- Exercise 5.
-- |
-- ndicates whether an Integer could be a valid credit card number.
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882
-- False

validate :: Integer -> Bool
validate = (==0) . lastDigit . sumDigits . doubleEveryOther . toDigits

--------------------------------------------------------------------------------
--- Exercise 6. The Towers of Hano

-- |
-- The Towers of Hanoi is a classic puzzle with a solution
-- that can be described recursively.
-- 3 pegs case
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"), ("a","b"), ("c","b")

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
--------------------------------------------------------------------------------
--- Exercise 7.

-- |
-- 4 pegs case

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined




