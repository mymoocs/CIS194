module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-isrc/cis194"
               , "src/cis194/Hw01.hs"
               , "src/cis194/Hw03_Golf.hs"
               , "src/cis194/Hw04.hs"
               , "src/cis194/Hw05_Calc.hs"]
