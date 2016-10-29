
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
-- Exercise 2

-- | generates a balanced binary tree from a list of values using foldr.
--
-- foldTree "ABCDEFGHIJ"
-- Node 3 (Node 2 (Node 0 Leaf 'F' Leaf) 'I' (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf)) 'J' (Node 2 (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf) 'H' (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
--
-- >>> isBalanced $ foldTree "ABCDEFGHIJ"
-- True
--
-- >>> isTreeBalanced $ foldTree "ABCDEFGHIJ"
-- True

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree = foldr insert Leaf


insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node n l y r)
  | hL > hR = Node (hR + 1) l y (insert x r)
  | otherwise = Node (hL + 1) (insert x l) y r
  where
    hR = treeHeight r
    hL = treeHeight l

treeHeight Leaf = 0
treeHeight (Node _ l _ r) =   1 + min (treeHeight l) (treeHeight r)
