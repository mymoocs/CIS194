module Main where

--import System.FilePath.Glob (glob)
import Test.DocTest(doctest)

main :: IO ()
main = do
    putStrLn "--- Start Doctests ---"

--    glob  "src/**/*.hs" >>= doctest
    doctest [ "-isrc/Cis194"
            , "src/Cis194/Hw01.hs"]
    putStrLn "--- End   Doctests ---"
