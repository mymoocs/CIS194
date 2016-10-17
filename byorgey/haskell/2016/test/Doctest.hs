module Main where

--import System.FilePath.Glob (glob)
import Test.DocTest(doctest)

main :: IO ()
main = do
    putStrLn "--- Start Doctests ---"
--    glob  "src/**/*.hs" >>= doctest
    doctest [ "-isrc/"
            , "src/Cis194/Hw01.hs"
            , "src/Cis194/Hw02/LogAnalysis.hs"
            ]
    putStrLn "--- End   Doctests ---"
