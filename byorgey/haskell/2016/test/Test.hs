{--
  Created       : 2016 Nov 27 (Sun) 03:39:14 PM by Arthur Vardanyan.
  Last Modified : 2017 Jan 05 (Thu) 04:58:39 PM by Arthur Vardanyan.
--}

import Test.Tasty

import Cis194.Hw07.JoinListTest
import Cis194.Hw08.PartyTest
import Cis194.Hw10.AParserTest
import Cis194.Hw11.SExprTest


main :: IO ()
main = defaultMain $ testGroup "All Test" tests

tests :: [TestTree]
tests = [week7Tests,   week8Tests, week10Tests, week11Tests]
