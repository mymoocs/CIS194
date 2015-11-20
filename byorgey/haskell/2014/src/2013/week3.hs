-- CIS 194 Homework 2
{-# OPTIONS_GHC -Wall #-}
module Golf where

skips :: [a] -> [[a]]
skips xs = skips' xs 0
skips' [] _ = []
skips' all@(x:xs) n = (gen all n) :  (skips' xs (n+1))

gen :: [a] -> Int -> [a]
gen xs 0 = xs
gen [] _ = []
gen (x:xs) n = x : gen (drop n xs) n