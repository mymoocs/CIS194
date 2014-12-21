{-# OPTIONS_GHC -Wall #-}
--Validating Credit Card Numbers
import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits n | n <= 0 = []
           |otherwise = map (toInteger . digitToInt) (show n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev =  getDigits --reverse .toDigits

getDigits :: Integer->[Integer]
getDigits n | n<10 = [n]
            | otherwise = (mod n 10) : getDigits (div n 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . otherTwo . reverse

otherOne ::Num a => [a]->[a]
otherOne [] = []
otherOne (x:xs) = (2*x) : otherTwo xs

otherTwo ::Num a=> [a]->[a]
otherTwo [] = []
otherTwo (x:xs) = x:(otherOne xs)


sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate xs = (mod (crs xs) 10) == 0
   
crs ::Integer -> Integer               
crs = sumDigits . doubleEveryOther . toDigits  



--Hanoi puzzle
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a














