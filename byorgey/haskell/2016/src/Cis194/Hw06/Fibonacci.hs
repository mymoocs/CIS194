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
interleaveStreams (x:.xs) ys = x :. interleaveStreams ys xs
