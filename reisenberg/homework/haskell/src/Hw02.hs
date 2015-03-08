{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}
{-# OPTIONS_GHC -Wall #-}

module Hw02 where

import Hw02_Words
import Data.List (elem, delete)

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:

--------------------------------------------------------------------------------
--- Exercise 1.

-- | 
-- sts whether a certain word is formable from the tiles
-- in a Scrabble hand.
-- >>> formableBy "fun" ['x','n','i','f','u','e','l']
-- True
-- >>> formableBy "haskell" ['k','l','e','h','a','l','s']
-- True
-- >>> formableBy "haskell" ['k','l','e','h','a','y','s']
-- False

formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (c:cs) hs |c `elem` hs = formableBy cs (delete c hs)
                     |otherwise = False

ex1 :: T.Test
ex1 = T.TestList
      [
        U.teq "ex10" (formableBy "fun" ['x','n','i','f','u','e','l']) True
      , U.teq "ex11" (formableBy "haskell" ['k','l','e','h','a','l','s']) True
      , U.teq "ex12" (formableBy "haskell" ['k','l','e','h','a','y','s']) False
      ]
--------------------------------------------------------------------------------
--- Exercise 2.

-- |
--
-- >>> wordsFrom ['a','b','c','d']
-- ["ab","ad","ba","bad","cab","cad","dab"]
-- >>> wordsFrom ['h','e','l','l','o'] 
-- ["eh","el","ell","he","hell","hello","helo","ho","hoe","hole","lo","oe","oh","ole"]

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

ex2 :: T.Test
ex2 = T.TestList
      [
        U.teq "ex20" (wordsFrom ['a','b','c','d']) (["ab","ad","ba","bad","cab","cad","dab"])
      , U.teq "ex21" (wordsFrom ['h','e','l','l','o']) (["eh","el","ell","he","hell","hello","helo","ho","hoe","hole","lo","oe","oh","ole"])      
      ]


hw2 :: IO T.Counts
hw2 = do
  T.runTestTT ex1
  T.runTestTT ex2
  
  
