module Hw04 where

-- Exercise 1: Wholemeal programming
-- Reimplement each of the following functions in a more idiomatic
-- Haskell style.
-- 1.1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x->x-2) . filter even


-- 1.2
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)



-- Exercise 2: Folding with trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

                     
-- Exercise 3: More folds!

-- 3.1
xor :: [Bool] -> Bool
xor = foldr (\a acc -> case (a, acc) of
                        (False, True) -> True
                        (True, False) -> True
                        (_, _) -> False) False

xor' :: [Bool] -> Bool
xor' = foldr (\acc a -> case (a, acc) of
                        (False, True) -> True
                        (True, False) -> True
                        (_, _) -> False) False
                        


-- 3.2
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) [] 

-- 3.3
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

-- Exercise 4: Finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram = undefined

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

