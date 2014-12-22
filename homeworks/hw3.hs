{-# OPTIONS_GHC -Wall  #-}

module Golf where


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

-- Exercise 3 Histogram
