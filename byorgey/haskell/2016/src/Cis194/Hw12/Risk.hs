{--
  Created       : 2017 Jan 07 (Sat) 07:15:55 PM by Arthur Vardanyan.
  Last Modified : 2017 Jan 15 (Sun) 07:44:42 AM by Arthur Vardanyan.
--}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cis194.Hw12Risk where
import Cis194.Hw11.SExpr
import Cis194.Hw10.AParser
import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first' :: (a -> b) -> (a, c) -> (b, c)
first' f (a, c) = (f a, c)

instance Random DieValue where
  random           = first' DV . randomR (1,6)
  randomR (low,hi) = first' DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


-- Lecture Samples
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (ma:mas) =
  ma >>= \a ->
  sequence' mas >>= \as ->
  return (a:as)


replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence' (replicate n m)

{--
parseFile :: Parser [[Int]]
parseFile = many parseLine

parseLine :: Parser [Int]
parseLine = parseInt >>= \i -> replicateM i parseInt
--}
-- 4 78 19 3 44 3 1 7 5 2 3 2
-- 78 19 3 44   -- first group
-- 1 7 5        -- second group
-- 3 2          -- third group
