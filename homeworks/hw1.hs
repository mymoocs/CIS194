{-# OPTIONS_GHC -Wall #-}

-- {-# LANGUAGE ScopedTypeVariables #-}

-- Validating Credit Card Numbers

-- 1.
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = map (\c -> read [c]) (show n)


toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits


-- 2.
dodd :: (a -> a) -> [a] -> [a]
dodd _  [] = []
dodd f (x:xs) =  f x : deven f xs

deven :: (a -> a) -> [a] -> [a]
deven _ [] = []
deven f (x:xs) =  x : dodd f xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = if m == 0
                      then dodd double xs
                      else deven double xs
                           where
                             m = mod (length xs) 2
                             double x = x * 2


-- 3.

sumDigits :: [Integer] -> Integer
sumDigits =  sum . map (sum . toDigits)

sumDigits1 :: [Integer] -> Integer
sumDigits1 =  sum . concat . map toDigits

-- 4. --

validate :: Integer -> Bool
validate =  (\x -> mod x 10 == 0) . sumDigits . doubleEveryOther . toDigits


-- The Towers of Hanoi

-- 5.
-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = undefined
-- hanoi n p1 p2 p3 =
