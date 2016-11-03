
-- week 4

module HigherOrder where

-------------------------------------------------------------------------------
-- Exercise 1 Wholemeal programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x->x-2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate seed
        where seed n = if even n then div n 2 else 3 * n + 1


-------------------------------------------------------------------------------
-- Exercise 2: Folding with trees

-- | generates a balanced binary tree from a list of values using foldr.
--
-- >>> foldTree "ABCDEFGHIJ"
-- Node 3 (Node 2 (Node 0 Leaf 'F' Leaf) 'I' (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf)) 'J' (Node 2 (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf) 'H' (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
--
-- isBalanced $ foldTree "ABCDEFGHIJ"
-- True
--
-- isTreeBalanced $ foldTree "ABCDEFGHIJ"
-- True

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree = foldr insert Leaf


insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ l y r)
  | tgt l r = Node (hR + 1) l y (insert x r)
  | otherwise = Node (hL + 1) (insert x l) y r
  where
    hR = treeDeep r
    hL = treeDeep l

treeDeep :: (Ord t, Num t) => Tree t1 -> t
treeDeep Leaf = 0
treeDeep (Node _ l _ r) = 1 + min (treeDeep l) (treeDeep r)

tgt :: Tree t -> Tree t1 -> Bool
tgt Leaf Node {} = False
tgt (Node _ _ _ _) Leaf = True
tgt Leaf Leaf = False
tgt (Node m _ _ _)  (Node k _ _ _) = m >= k

treeHeight :: Tree t -> Integer
treeHeight Leaf = 0
treeHeight (Node n _ _ _) = n


-------------------------------------------------------------------------------
-- Exercise 3

-- 1)
-- | returns True if and only if there are an odd number of True
-- values contained in the input list.
--
-- >>> xor [False, True, False]
-- True
--
-- >>> xor [False, True, False, False, True]
-- False
xor :: [Bool] -> Bool
xor = xor' False
    where
      xor' z [] = z
      xor' z (x:xs) = xor' (if x then not z else z ) xs


-- >>> xor' [False, True, False]
-- True
--
-- >>> xor' [False, True, False, False, True]
-- False
xor' :: [Bool] -> Bool
xor' = foldr (\acc x -> (if x then not acc else acc )) False

-- 2)
-- | Implement map as a fold.
--
-- >>> map' (+2) [1..3]
-- [3,4,5]
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- 3)
-- | Implement foldl using foldr.
-- >>> let ps = "((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)"
-- >>> myFoldl (\x y -> concat ["(", x,"+",y,")"]) "0" $ map show [1..10]
-- "((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)"
-- >>>  foldl (\x y -> concat ["(", x,"+",y,")"]) "0" $ map show [1..10]
-- "((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)"
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f z xs = foldr (flip f) z (reverse xs)

-------------------------------------------------------------------------------
-- Exercise 4 Finding primes
-- | Implement the algorithm using function composition. Given an integer n,
-- your function should generate all the odd prime numbers up to 2n + 2.

sieveSundaram :: Integer -> [Integer]
sieveSundaram = undefined
