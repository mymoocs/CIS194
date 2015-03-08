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

import qualified Test.HUnit          as T
import qualified Test.HUnit.Util     as U

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


ex1 :: T.Test
ex1 = T.TestList
      [
        U.teq "ex1.10" (lastDigit 123) 3
      , U.teq "ex1.11" (lastDigit 0) 0
      , U.teq "ex1.20" (dropLastDigit 123) 12
      , U.teq "ex1.21" (dropLastDigit 1) 0
      ]

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

ex2 :: T.Test
ex2 = T.TestList
      [
        U.teq "ex20" (toDigits 123) [1,2,3]
      , U.teq "ex21" (toDigits 0) []
      , U.teq "ex22" (toDigits (-17)) []      ]
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

ex3 :: T.Test
ex3 = T.TestList
      [
        U.teq "ex30" (doubleEveryOther [8,7,6,5]) [16,7,12,5]
      , U.teq "ex31" (doubleEveryOther [1,2,3])  [1,4,3]
      ]


--------------------------------------------------------------------------------
--- Exercise 4.
-- | calculate the sum of all digits in the list of integer numbers.
-- >>> sumDigits [16,7,12,5] == 1 + 6 + 7 + 1 + 2 + 5
-- True

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

ex4 :: T.Test
ex4 = T.TestList
      [
        U.teq "ex40" (sumDigits [16,7,12,5]) (1 + 6 + 7 + 1 + 2 + 5)
      , U.teq "ex41" (sumDigits [16,33,1,5]) (1 + 6 + 3 + 3 + 1 + 5)
      ]
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

ex5 :: T.Test
ex5 = T.TestList
      [
        U.teq "ex50" (validate 4012888888881881) True
      , U.teq "ex51" (validate 4012888888881882) False
      ]

--------------------------------------------------------------------------------
--- Exercise 6. The Towers of Hano

-- |
-- The Towers of Hanoi is a classic puzzle with a solution
-- that can be described recursively.
-- 3 pegs case
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"), ("a","b"), ("c","b")]

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ (move a b) ++ hanoi (n-1) c b a

move :: Peg -> Peg -> [Move]
move a b = [(a,b)] 

ex6 :: T.Test
ex6 = T.TestList
      [
        U.teq "ex6" (hanoi 2 "a" "b" "c") [("a","c"), ("a","b"), ("c","b")]
      ]
--------------------------------------------------------------------------------
--- Exercise 7.

-- |
-- 4 pegs case

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined

ex7 :: T.Test
ex7 = T.TestList
      [
        
      ]

--------------------------------------------------------------------------------
--- Hanoi problem with advanced data structures
-- | 

data Peg' = A | B | C deriving (Eq)


instance Show Peg' where
  show A = "a"
  show B = "b"
  show C = "c"
  
type Move' = (Peg', Peg')
hanoi' :: Integer -> Peg' -> Peg' -> Peg' -> [Move']
hanoi' 0 _ _ _ = []
hanoi' n a b c = hanoi' (n-1) a c b ++ (move' a b) ++ hanoi' (n-1) c b a

move' :: Peg' -> Peg' -> [Move']
move' a b = [(a,b)]

a :: Peg'
a = A

b :: Peg'
b = B

c :: Peg'
c = C


ex8 :: T.Test
ex8 = T.TestList
      [
        U.teq "ex80" (hanoi' 3 a b c) [(a,b),(a,c),(b,c),(a,b),(c,a),(c,b),(a,b)]
      ]


      
--------------------------------------------------------------------------------
--- Hw 01 test

hw1 :: IO T.Counts
hw1 = do
  T.runTestTT ex1
  T.runTestTT ex2
  T.runTestTT ex3
  T.runTestTT ex4
  T.runTestTT ex5
  T.runTestTT ex6
  T.runTestTT ex7
  T.runTestTT ex8



