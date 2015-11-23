{-# OPTIONS_GHC -fforce-recomp #-}

module Main (main) where


import           Criterion.Main (bgroup, defaultMain)
import qualified Cis194.Hw01Bench
import qualified Cis194Bench

main :: IO ()
main = defaultMain
    [ bgroup "Cis194" Cis194Bench.benchmarks
    , bgroup "Cis194.Hw01" Cis194.Hw01Bench.benchmarks
    ]
