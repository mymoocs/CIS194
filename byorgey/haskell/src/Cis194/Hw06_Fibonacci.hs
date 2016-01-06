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


------------------------------------------------------------------------------
-- Exercise 7:
