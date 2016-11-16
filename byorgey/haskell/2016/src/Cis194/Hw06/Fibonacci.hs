{-# LANGUAGE FlexibleInstances #-} -- for ex 6
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- http://www.cs.nott.ac.uk/~pszgmh/contractive.pdf
--  Representing Contractive Functions on Streams GRAHAM HUTTON

-------------------------------------------------------------------------------
-- Exercise 1.

{--
F0 = 0
F1 = 1
Fn = Fn−1 + Fn−2 (n ≥ 2)--}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1  = map fib [0..]

-------------------------------------------------------------------------------
-- Exercise 2

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)



-- Streams

-------------------------------------------------------------------------------
-- Exercise 3

infix 5 :.
data Stream a = a :. Stream  a

streamToList :: Stream a -> [a]
streamToList (s:.ss) = s : streamToList ss

instance Show a => Show (Stream a) where
--    show = show . take 20 . streamToList
    show ss = show' ss 20
        where
          show' :: Show a => Stream a -> Integer -> String
          show' _ 0 = "..."
          show' (e:.s) n = show e ++","++ show' s (n-1)


-------------------------------------------------------------------------------
-- Exercise 4
redstream :: (a -> b -> b) -> Stream a -> b
redstream f (s:.ss) = f s  (redstream f ss)


streamRepeat :: a -> Stream a
streamRepeat s = s :. streamRepeat s

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f  = redstream  ((:.) . f)

ones :: Stream Int
ones = streamRepeat 1


streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f z = (f z) :. streamFromSeed f (f z)


-------------------------------------------------------------------------------
-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- this nice solution found here
-- https://mail.haskell.org/pipermail/beginners/2014-February/013158.html
ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])


zeros :: Stream Integer
zeros = streamRepeat 0

interleaveStreams :: Stream a -> Stream a -> Stream a
-- this is not lazy on right argument due to pattern matching
-- interleaveStreams (x:.xs) (y:.ys) = x :. (y :. interleaveStreams xs ys)
-- we can make the second argument be pattern-matched lazily with ~.
-- interleaveStreams (x:.xs) ~(y:.ys) = x :. (y :. interleaveStreams xs ys)
interleaveStreams (a:.as) ys = a :. interleaveStreams ys as

-- |
-- Fibonacci numbers via generating functions

-- | We will use streams of Integers to compute the Fibonacci numbers.
-- a0 + a1x + a2x^2 + · · · + anx* n + . . .

-------------------------------------------------------------------------------
-- Exercise 6

x :: Stream Integer
x = 0 :. (1 :. streamRepeat 0)

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (a:.as) ~(b:.bs) = c :. streamZipWith f as bs
    where
      c = f a b

instance Num (Stream Integer) where
    fromInteger n = n :. streamRepeat 0
    negate = streamMap (*(-1))
    (+) = streamZipWith (+)
    (*) (a:.as) allB@(b:.bs) = c0 :. (aB' + as * allB)
                              where
                                c0 = a * b
                                aB' = streamMap (*a) bs
instance Fractional (Stream Integer) where
    (/) allA@(a:.as) allB@(b:.bs) = a `div` b :. streamMap ( `div` b) (as - q * bs )
                                     where
                                       q = allA / allB


-- |
-- Consider representing the Fibonacci numbers using a generating function.
-- F(x) = F0 + F1x + F2x^2 + F3x^3 + . . .
-- Notice that x + xF(x) + x^2F(x) = F(x):

fibs3 :: Stream Integer
fibs3  = x / (1 - x - (x*x))



-- | Fibonacci numbers via matrices (extra credit)

-------------------------------------------------------------------------------
-- Exercise 7



data Matrix = Matrix Integer Integer Integer Integer

instance Show Matrix where
    show (Matrix a11 a12 a21 a22) =
        concat ["|", show a11, " ", show a12, "|"] ++ "\n"
     ++ concat ["|", show a21, " ", show a22, "|"]


instance Num Matrix where
    (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22)
        =  Matrix (a11*b11 + a12*b12) (a11*b12 + a12*b22)
                  (a21*b11 + a22*b21) (a21*b12 + a22*b22)

f :: Matrix
f = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = let Matrix _ fib _ _ = f ^ n in fib
