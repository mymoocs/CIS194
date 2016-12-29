{--
  Created       : 2016 Nov 27 (Sun) 03:39:14 PM by Arthur Vardanyan.
  Last Modified : 2016 Nov 27 (Sun) 03:39:41 PM by Arthur Vardanyan.
--}

import Test.Tasty

import Cis194.Hw07.JoinListTest
import Cis194.Hw08.PartyTest
import Cis194.Hw10.AParserTest


main :: IO ()
main = defaultMain $ testGroup "All Test" tests

tests :: [TestTree]
tests = [week7Tests,   week8Tests, week10Tests]
