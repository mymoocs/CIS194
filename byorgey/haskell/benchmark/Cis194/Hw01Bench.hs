module Cis194.Hw01Bench (benchmarks) where

import           Criterion
import           Cis194.Hw01

benchmarks :: [Benchmark]
benchmarks =
      [ bgroup "toDigits"
        [ bench "3 digit input" $ whnf toDigits 123
        , bench "8 digit input" $ whnf toDigits 12345678
        , bench "negative input" $ whnf toDigits (-12345678)
        , bench "0" $ whnf toDigits 0  
        ]
      ]
