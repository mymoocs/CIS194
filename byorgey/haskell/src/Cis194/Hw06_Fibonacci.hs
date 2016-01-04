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

------------------------------------------------------------------------------
-- Exercise 5:

------------------------------------------------------------------------------
-- Exercise 6:

------------------------------------------------------------------------------
-- Exercise 7:
