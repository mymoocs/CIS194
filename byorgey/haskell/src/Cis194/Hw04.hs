module Cis194.Hw04
    (foldTree
    , isBalanced
    , isTreeBalanced
    , Tree(..)
    , xor)
    where


------------------------------------------------------------------------------
-- Exercise 1: Wholemeal programming


------------------------------------------------------------------------------
-- Exercise 2: Folding with trees

-- | generates a balanced binary tree from a list of values using foldr.
-- foldTree "ABCDEFGHIJ"
-- Node 3 (Node 2 (Node 0 Leaf 'F' Leaf) 'I' (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf)) 'J' (Node 2 (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf) 'H' (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
-- >>> isBalanced $ foldTree "ABCDEFGHIJ"
-- True
-- >>> isTreeBalanced $ foldTree "ABCDEFGHIJ"
-- True      
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: Ord a => [a] -> Tree a
foldTree  = foldr insertInTree Leaf

insertInTree :: a -> Tree a -> Tree a
insertInTree x Leaf = Node 0 (Leaf) x (Leaf)
insertInTree x (Node n t1 val t2)
    | h1 < h2   = Node (h2+1) (insertInTree x t1) val t2
    | otherwise = Node (h1+1) t1 val (insertInTree x t2)
  where h1  = heightTree t1
        h2  = heightTree t2

heightTree :: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node n _ _ _) = n

treeMaxHeight (Node _ l _ r) = max (heightTree l) (heightTree r)
treeMinHeight (Node _ l _ r) = min (heightTree l) (heightTree r)

-- for tree structure which save height in each node
isTreeBalanced :: Tree a -> Bool
isTreeBalanced Leaf = True
isTreeBalanced (Node _ l _ r) = isNodeBalanced l && isNodeBalanced r

isNodeBalanced :: Tree a -> Bool                                
isNodeBalanced tree = (maxH - minH)  <= 1
    where
      maxH = treeMaxHeight tree
      minH = treeMinHeight tree

-- for any type of tree    
isBalanced   = (/= -1) . checkBalanced
checkBalanced Leaf = 0
checkBalanced (Node _ l _ r)
    | left  == -1 = -1
    | right == -1 = -1
    | delta >   1 = -1
    | otherwise   = 1 + max left right   
    where
      left  = checkBalanced l
      right = checkBalanced r
      delta = abs (left-right)

              
------------------------------------------------------------------------------
-- Exercise 3: More folds!
-- | which returns True if and only if there are an odd number of True
--   values contained in the input list. It does not matter how many
--   False values the input list contains.
--   Your solution must be implemented using a fold.
--
-- >>> xor [False, True, False]
-- True
-- >>> xor [False, True, False, False, True]
-- False
                                                                              
{-
\ f z list ->
  var r = z
  foreach ( e in list )
    r = f (r , e )
  return r
-}

xor :: [Bool] -> Bool
xor = foldl xor' False

xor' a b
     | b          = not a
     | otherwise  = a
                    
------------------------------------------------------------------------------
-- Exercise 4: Finding primes
      
      
      
