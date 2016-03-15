module Main (main) where

import           Data.List      (genericLength)
import           Data.Maybe     (catMaybes)
import           System.Exit    (exitFailure, exitSuccess)
import           System.Process (readProcess)
import           Text.Regex     (matchRegex, mkRegex)

arguments :: [String]
arguments =
  [ "report"
      -- , ".stack_work/dist/hpc/tix/hspec/hspec.tix"
  , "./hpctixdir/cis194-hspec-44310.tix"
--    , "./.stack-work/dist/x86_64-osx/Cabal-1.22.4.0/hpc/cis194-hspec.tix"
  ]

average :: (Fractional a, Real b) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

expected :: Fractional a => a
expected = 80

main :: IO ()
main = do
  output <- readProcess "hpc" arguments ""
  if average (match output) >= expected
    then exitSuccess
    else putStr output >> exitFailure

match :: String -> [Int]
match = fmap read . concat . catMaybes . fmap (matchRegex pattern) . lines
  where
    pattern = mkRegex "^ *([0-9]*)% "
