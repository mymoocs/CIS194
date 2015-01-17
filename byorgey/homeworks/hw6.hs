{-# OPTIONS_GHC -Wall #-}

-- about Haskell's lazy evaluation
-- the ability to work with infinite data structures.
module Hw6 where

-- Fibonacci numbers

-- 1.
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Ex 2
fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)


-- Ex 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons e s) = e :streamToList s

ones :: Stream Int
ones = Cons 1 ones

instance Show a => Show (Stream a) where
  show stream = show' stream 20
    where
      show' _ 0 = "..."
      show' (Cons e s) n = show e ++","++ show' s (n-1)

-- streamToList (x:xs) = Cons x (streamToList xs)

-- Ex 4.1
streamRepeat :: a -> Stream a
streamRepeat e = Cons e (streamRepeat e)

-- Ex 4.2
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons e s) = Cons (f e) (streamMap f s)

-- Ex 4.3
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f e = Cons e (streamFromSeed f (f e))

-- Ex 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = undefined
