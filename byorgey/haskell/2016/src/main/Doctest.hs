module Main where

import Test.DocTest(doctest)

main :: IO ()
main = do
    putStrLn "--- Start Doctests ---"
    doctest ["-isrc/Cis194"
               , "src/Cis194/Hw01.hs"]
    putStrLn "--- End   Doctests ---"
