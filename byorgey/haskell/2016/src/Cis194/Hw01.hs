module Cis194.Hw01 where

import Data.Char (digitToInt)

------------------------------------------------------------------------------
-- Exercise 1

-- | toDigits should convert positive Integers to a list of digits.
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


-- | should do the same as toDigits, but with the digits reversed
-- >>> toDigitsRev 1234
-- [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev  = reverse . toDigits



-- | toDigits should convert positive Integers to a list of digits.
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
toDigits' n,
    | n <= 0 = []
    | otherwise = toDigitsExceptLast' n ++ [m]
    where
      m = mod n 10

-- | point free syntax
toDigitsExceptLast' :: Integer -> [Integer]
toDigitsExceptLast'  = toDigits' . flip div 10

toDigitsExceptLast'' :: Integer -> [Integer]
toDigitsExceptLast'' n = toDigits' m where m = div n 10


-------------------------------------------------------------------------------
-- Exercise 2
