module Cis194.Hw03.Golf  where


import Data.List (find, group, sort)
-------------------------------------------------------------------------------
-- Exercise 1

-- | The first list in the output should
-- be the same as the input list. The second list in the output should
-- contain every second element from the input list. . . and the nth list in
-- the output should contain every nth element from the input list.

-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
--
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
--
-- >>> skips [1]
-- [[1]]
--
-- >>> skips [True,False]
-- [[True,False],[False]]
--
-- >>> skips []
-- []

skips :: [a] ->[[a]]
skips [] = []
skips [a] = [[a]]
skips xs = filter (not . null) $ s 0 xs
           where
             len = length xs
             s n ys
               | n > len = []
               | otherwise = skip n ys : s (n+1) xs

skip :: Int -> [a] -> [a]
skip 0 xs = xs
skip n xs =  case drop n xs of
               []     -> []
               (y:ys) -> y : skip n ys


-------------------------------------------------------------------------------
-- Exercise 2

-- | A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it.
--
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
--
-- >>> localMaxima [2,3,4,1,5]
-- [4]
--
-- >>> localMaxima [1,2,3,4,5]
-- []

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:xs)
    | x < y && y > z = y : localMaxima (y:z:xs)
    | otherwise      = localMaxima (y:z:xs)


-------------------------------------------------------------------------------
-- Exercise 3
-- histogram :: [Integer] -> String
histogram xs = [ case find (\ (a,_) -> i==a) g of
                Just (_, n) -> pr n ++ "\n"
                Nothing     -> []
                            | i <- [0..9]]
            where
              g = map (\xs -> (head xs, length xs)) $ group $ sort xs -- [1,2,3,4,1,2,3,4]

p n = find (\(a,_) -> a == n) $  map (\xs -> (head xs, length xs)) $ group $ sort [1,2,3,4,1,2,3,4]

pr n = take n (repeat '*') ++ take (10-n) (repeat ' ')
