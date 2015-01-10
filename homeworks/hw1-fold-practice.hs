module FoldPractice where

myFoldr _ s [] = s
myFoldr f s (x:xs) = f x (myFoldr f s xs)  


-- wrong
--compare result with output of ex2.
myFoldl _ s [] = s
myFoldl f s (x:xs) = f (myFoldl f s xs) x

myFoldl1 _ s [] = s
myFoldl1 f s (x:xs) = let s' = f s x
                      in myFoldl1 f s' xs

myFoldl1' _ s [] = s
myFoldl1' f s (x:xs) = let s' = f s x
                      in seq s' $ myFoldl1' f s' xs                         

myFoldrl _ s [] = s
myFoldrl f s xs = foldr (flip f) s xs


folHhead = foldr (\a b->a) undefined 
foldLast = foldl (\a b->b) undefined 



-- https://www.haskell.org/haskellwiki/Fold

-- Both finite and indefinitely defined lists can be folded over in a tree-like fashion:

foldt            :: (a -> a -> a) -> a -> [a] -> a
foldt f z []     = z
foldt f z [x]    = x
foldt f z xs     = foldt f z (pairs f xs)
 
foldi            :: (a -> a -> a) -> a -> [a] -> a
foldi f z []     = z
foldi f z (x:xs) = f x (foldi f z (pairs f xs))
 
pairs            :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:t)  = f x y : pairs f t
pairs f t        = t


-- self explanatori outputs
ex1 = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])
 -- output:  "(1+(2+(3+(4+(5+(6+(7+(8+(9+(10+(11+(12+(13+0)))))))))))))"

ex2 = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])
-- output: "(((((((((((((0+1)+2)+3)+4)+5)+6)+7)+8)+9)+10)+11)+12)+13)"

ex3 = foldt (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])
-- output: "((((1+2)+(3+4))+((5+6)+(7+8)))+(((9+10)+(11+12))+13))"

ex4 = foldi (\x y -> concat ["(",x,"+",y,")"]) "0" (map show [1..13])
-- "(1+((2+3)+(((4+5)+(6+7))+((((8+9)+(10+11))+(12+13))+0))))"
