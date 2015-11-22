module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc/cis194", "src/cis194/Hw01.hs"]
