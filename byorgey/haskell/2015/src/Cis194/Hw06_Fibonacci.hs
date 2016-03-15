{-# LANGUAGE FlexibleInstances #-} -- for ex 6.
-- | ghc will complain that you haven’t defined them
{-# OPTIONS_GHC -fno-warn-missing-methods #-} 

module Hw06_Fibonacci
      (fib
      , fibs1
      , fibs2)
      where

-- 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, . . .


------------------------------------------------------------------------------
-- Exercise 1:
fib :: Integer -> Integer
fib = undefined       

-- fib has running time O(Fn), which (it turns out) is equivalent to O(ϕ^n),
-- where ϕ = (1+√5)/2 is the “golden ratio”.
fibs1 :: [Integer]
fibs1 = map fib [0..]         
      
------------------------------------------------------------------------------
-- Exercise 2:

-- requires only O(n) addition operations
fibs2 :: [Integer]
fibs2 = undefined
        
------------------------------------------------------------------------------
-- Exercise 3:

data Stream a = a :. Stream a
infixr 5 :.              
instance (Show a) => Show (Stream a) where
--   show ss = show . take 20 $ streamToList ss
   show ss = show' ss 20
       where
         show' :: Show a => Stream a -> Integer -> String
         show' _ 0 = "..."
         show' (e:.s) n = show e ++","++ show' s (n-1)
                                                                                      
              
streamToList :: Stream a -> [a]
streamToList (x:.xs) = x:streamToList xs

------------------------------------------------------------------------------
-- Exercise 4:

-- | a function which generates a stream containing infinitely many copies of the
--   given element.

streamRepeat :: a -> Stream a
streamRepeat e = e:.streamRepeat e

-- | a function which applies a function to every element of a Stream.
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (s:.ss) = f s :. streamMap f ss

-- | a function which generates a Stream from a “seed” of type a, which is the first
-- element of the stream, and an “unfolding rule” of type a -> a which specifies how
-- to transform the seed into a new seed, to be used for generating the rest of the stream. 
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = a:.streamFromSeed f (f a)

------------------------------------------------------------------------------
-- Exercise 5:
-- Define the streams

-- | which contains the infinite list of natural numbers 0, 1, 2, . . .
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- | which corresponds to the ruler function 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,...
ruler :: Stream Integer
ruler = interleaveStreams zeros maxPowers2
        where
          maxPowers2 = streamMap maxDivisor evens
                       
evens :: Stream Integer
evens = streamFromSeed (+ 2) 2
        
zeros :: Stream Integer         
zeros = streamRepeat 0
        
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (a:.as) (b:.bs) = a:.b:.interleaveStreams as bs 

                                    
power2 = [ 2^x | x <- [1..] ]

maxDivisor :: Integer -> Integer         
maxDivisor = maxDivisor' 0
    where
      maxDivisor' :: Int  -> Integer -> Integer
      maxDivisor' i n
           | mod n (power2 !! i) /= 0 = fromIntegral i
           | otherwise = maxDivisor' (i+1) n
                         
------------------------------------------------------------------------------
-- Exercise 6:

x :: Stream Integer
x = 0:.1:.zeros

instance Num (Stream Integer) where
    fromInteger = (:.zeros)
    negate = streamMap negate
    (+) (a:.as) (b:.bs)       = a+b :. as + bs
    (*) (a:.as) b'@(b:.bs)    = a*b :. (streamMap (* a) bs) + as * b'

instance Fractional (Stream Integer) where                                
    (/) a'@(a:.as) b'@(b:.bs) = a `div` b :. streamMap (`div` b) (as - (a' / b') * bs)


fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

------------------------------------------------------------------------------
-- Exercise 7:
data Matrix = Matrix Integer Integer Integer Integer

instance Show Matrix where
    show (Matrix a11 a12 a21 a22) = 
                                  "|" ++ show a11 ++ " " ++ show a12 ++ "|\n" ++
                                  "|" ++ show a21 ++" " ++ show a22 ++ "|"    

instance Num Matrix where
    (*) (Matrix  a11 a12 a21 a22) (Matrix b11 b12 b21 b22) = Matrix c11 c12 c21 c22
                    where
                      c11 = a11 * b11 + a12 * b21
                      c12 = a11 * b21 + a12 * b22
                      c21 = a21 * b11 + a22 * b21
                      c22 = a21 * b21 + a22 * b22
                            
mF :: Matrix
mF = Matrix 1 1 1 0
     
fib4 :: Integer -> Integer     
fib4 n = let (Matrix _ m _ _) =  mF ^ n
         in m
     
fib4' :: Integer -> Integer
fib4' = fib' mF
    where
      fib' (Matrix _ m _ _) 1 = m
      fib' m k = fib' (m*mF) (k-1)

