#bin/bash
echo "---- doctests ---"
echo "Week 01"
doctest -isrc src/Cis194/Hw01.hs
echo "Week 02"
doctest -isrc src/Cis194/Hw02/LogAnalysis.hs
echo "Week 03"
doctest -isrc src/Cis194/Hw03/Golf.hs
echo "Week 04"
doctest -isrc src/Cis194/Hw04/HigherOrder.hs
echo "Week 05"
doctest -isrc src/Cis194/Hw05/Calc.hs
echo "Week 06"
doctest -isrc src/Cis194/Hw06/Fibonacci.hs


echo "--- other tesrs ---"
stack runhaskell --  -isrc -itest test/Cis194/Hw01/HSpec.hs
stack runhaskell --  -isrc -itest test/Cis194/Hw01/UTest.hs
stack runhaskell --  -isrc -itest test/Cis194/Hw02/LogAnalysisUTest.hs
stack runhaskell --  -isrc -itest test/Cis194/Hw03/GolfSpec.hs

find src -name '*.hs' | xargs graphmod -q | dot -omodule-structure.dot
#dot -Tps module-structure.dot -o outfile.ps


echo "-----------------------------------"
