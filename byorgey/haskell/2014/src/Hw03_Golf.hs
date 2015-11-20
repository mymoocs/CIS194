{-# OPTIONS_GHC -Wall  #-}


module Hw03_Golf where
import Data.List

{- skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1] == [[1]]
skips [True,False] == [[True,False], [False]]
skips [] == [] -}

-- Exercise 1 Hopscotch
skips :: [a] -> [[a]]
skips xs =
  skips' xs 0 len
  where
    len = length xs
    skips' ys i n | i == n = []
                  |otherwise = everyNth i ys : skips' ys (i+1) n

everyNth1 :: Int -> [a] -> [a]
everyNth1 0 xs = xs
everyNth1 n xs =
  let
    ds = drop n xs
  in
   if null ds
   then []
   else  head ds : everyNth1 n (drop (n+1) xs)



-- other approach for everyNth

everyNth :: Int -> [a] -> [a]
everyNth 0 _  = []
everyNth 1 ys = ys
everyNth k ys = everyNth' 1 k ys
  where
    everyNth' :: Int -> Int -> [a] -> [a]
    everyNth' _ _ [] = []
    everyNth' cur n (x:xs)
      |cur == n = x : everyNth' 1 n xs
      |otherwise = everyNth' (cur+1) n xs
    


-- Exercise 2 Local maxima

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_ , _] = []
localMaxima xs@(_:x':xs') =
  [ b | (a,b,c) <- zip3 xs (x':xs') xs', a < b && b > c]

{--localMaxima' (a:b:c:cs)
  |a < b && b > c = b : localMaxima' (b:c:cs)
  |otherwse = localMaxima' (b:c:cs)
--}

-- Actually this is wrong if consider function types
-- nedd to process [Integer] not [int], i guess if
-- focuse on type to make correct will lead to find other solution,
-- more simple, which i found from interet later. 
-- Exercise 3 Histogram
histogram :: [Int] -> String
histogram =unlines .  histogram''

histogram'' :: [Int] -> [[Char]]
histogram'' xs =  transpose $ hist cs mx 0
            where
              cs = counts xs
              mx = maximum (map (\(_,n)->n) cs)

hist :: [(Int, Int)] -> Int -> Int -> [[Char]]
hist _ _ 10 = []
hist [] m n = emptyLine m n :hist [] m (n+1)
hist xs@((k,r):xs') m n
  | k == n = (blanks m r ++ asterix r) : hist xs' m (n+1)
  | otherwise = emptyLine m n : hist xs m (n+1)

counts :: [Int] -> [(Int, Int)]
counts xs = map (\ys -> (head ys, length ys))
            $ groupBy (\ a b -> a == b) (sort xs)

asterix :: Int  -> String
asterix n = replicate  n '*' ++ lineSuffix n

lineSuffix ::  Show a => a -> [Char]
lineSuffix n = "=" ++ (show n)

blanks :: Int -> Int -> String
blanks m n = replicate (m-n) ' ' 

emptyLine :: Show a => Int -> a -> [Char]
emptyLine m n = replicate m ' ' ++  lineSuffix n 

-- the solution process made a lot aux exploration functions

histogram1 :: [Int] -> String
histogram1 = unlines . histogram' . counts
            where
  --            histogram' ::  [(Integer, Int)] -> [String]
              histogram' xs =
                case xs of
                 [] -> []
                 ((k,n):xs') -> (asterixline k n 10) : histogram' xs'

hs :: [Int]
hs = [1,2,3,1,2,3,1,2,5,6,5,3,2,1]



histy :: (Eq a, Show a) => [(a, Int)] -> a -> [String]
histy [] n = [show n]
histy ((k,r):_) n
  | k == n = [replicate r '*']
  | otherwise = [replicate 10 '-']
             

--asterixline :: Integer -> Int -> Int -> String
asterixline :: (Enum a, Eq a, Num a, Show a) => a -> Int -> Int -> [Char]
asterixline k n m
  |k `elem` [0..9] =(show k)++'=': replicate (m-n) ' ' ++ replicate n '*'
  |otherwise = (show k)++'=':replicate m ' '

genStars :: (Enum t, Num t) => (t,t) -> [Char]
genStars (a,b) = "*" ++ [' ' | _<- [a+1..b-1] ]++"*"


pGen :: (Enum t1, Eq t, Num t, Num t1) => [(t1, t)] -> [Char]
pGen [] = []
pGen [_] = []
pGen ((_, 0) : xs) = pGen xs
pGen ((a, n) : (b,m) : xs) = genStars (a,b) ++"\n" ++
                             pGen ((a,n-1): (b, m-1):xs)



-- [SPOILERS]
-- cery nice solution from internet
histogrambest :: [Integer] -> String
histogrambest xs = (unlines . reverse) (buildhisto xs)
                   ++ "==========\n0123456789"

buildhisto :: [Integer] -> [String]
buildhisto [] = []
buildhisto xs = histoline xs : buildhisto xs'
  where xs' = xs \\ [0..9]

histoline :: [Integer] -> String
histoline xs = map mark [0..9]
  where mark x | x `elem` xs = '*'
               | otherwise = ' '


