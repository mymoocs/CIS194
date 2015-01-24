{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- about Haskell's lazy evaluation
-- the ability to work with infinite data structures.
module Hw06 where

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
      show' :: Show a => Stream a -> Integer -> String
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

-- Ex 5.1
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- uax function
zeroes :: Stream Integer
zeroes = streamFromSeed id 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x s1) (Cons y s2) = (Cons x
                                             (Cons y (interleaveStreams s1 s2)))
-- Ex 5.2
ruler :: Stream Integer
ruler = interleaveStreams zeroes powers2
  where
    evens :: Stream Integer
    evens = streamFromSeed (+2) 2
    
    powers2 :: Stream Integer
    powers2 = streamMap findPower2 evens

    findPower2 :: Integer -> Integer
    findPower2 n | odd n = 0
                 | otherwise = let d = n `div` 2 in 1 + findPower2 d


-- 6.

x :: Stream Integer
x = Cons 0 (Cons 1 (streamFromSeed id 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  -- negate (Cons n s) = Cons (-n) (negate s)
  negate = streamMap negate
  (+) (Cons n s1) (Cons m s2) = Cons (n+m) (s1 + s2)
  (*) (Cons n s1) s'@(Cons m s2) = Cons (n*m)
                                   --(((fromInteger n) * s2) + (s1 * s')) -- slow 
                                   ((streamMap (* n) s2) + (s1 * s'))

instance Fractional (Stream Integer) where
  (/) s@(Cons n s1) s'@(Cons m s2) = Cons (n `div` m)
                                     (streamMap (`div` m) (s1 - (s / s') * s2))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x*x)


-- 7.

data Matrix = Matrix Integer Integer Integer Integer
            deriving Show

instance Num Matrix where
  fromInteger n = Matrix n n n n
  negate (Matrix a b c d) = Matrix (-a) (-b) (-c) (-d)
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
    Matrix c11 c12 c21 c22
    where
      c11 = a11 * b11 + a12 * b21
      c12 = a11 * b12 + a12 * b22
      c21 = a21 * b11 + a22 * b12
      c22 = a21 * b12 + a22 * b22

mF :: Matrix 
mF = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 n = let (Matrix _ fibN _ _ )  = mF ^ n
         in fibN
