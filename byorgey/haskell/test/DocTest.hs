module Main (main) where

import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-isrc/cis194", "src/cis194/Hw01.hs"]
