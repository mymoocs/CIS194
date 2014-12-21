{-# OPTIONS_GHC -Wall  #-}

module Golf where


{- skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1] == [[1]]
skips [True,False] == [[True,False], [False]]
skips [] == [] -}

-- 1.
skips :: [a] -> [[a]]
skips xs =
  skips' xs 0 l
  where
    l = length xs
    skips' ys i n | i == n = []
                  |otherwise = (everyNth i ys) : skips' ys (i+1) n

everyNth1 :: Int -> [a] -> [a]
everyNth1 0 xs = xs
everyNth1 n xs =
  let
    ds = drop n xs
  in
   if null ds
   then []
   else  head ds : everyNth1 n (drop (n+1) xs)

everyNth2 :: Int -> [a] -> [a]
everyNth2 0 xs = xs
everyNth2 n xs =
  let
    ds@(y:ys) = drop n xs
  in
   if null ds
   then []
   else  y : everyNth2 n (drop n ds)


-- other approach for everyNth

everyNth :: Int -> [a] -> [a]
everyNth 0 _  = []
everyNth 1 xs = xs
everyNth n xs = everyNth' 1 n xs
  where
    everyNth' :: Int -> Int -> [a] -> [a]
    everyNth' _ _ [] = []
    everyNth' cur n (x:xs)
      |cur == n = x : everyNth' 1 n xs
      |otherwise = everyNth' (cur+1) n xs
    
