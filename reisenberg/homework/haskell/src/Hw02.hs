{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}
{-# OPTIONS_GHC -Wall #-}

module Hw02 where

import Hw02_Words
import Data.List (delete, sortBy, groupBy)

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


--------------------------------------------------------------------------------
--- Exercise 3.
-- |
-- Write a function wordFitsTemplate that checks to see if a given
-- word matches a template, given a set of tiles available.
-- >>> wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care"
-- True
-- >>> wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care"
-- False
-- >>> wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car"
-- False
-- >>> wordFitsTemplate "let" ['x','x'] "let"
-- True

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate temp hs word
  |temp == word = True
  |length temp /= length word = False
  |any (\(a,b) -> a /= b) cs = False
  |formableBy tw hs = True
  |otherwise = False
   where
     ps = zip temp word
     cs = filter (\(a,_) -> a /= '?') ps
     tw = map (\(_,b) -> b) $ filter (\(a,_) -> a == '?') ps

ex3 :: T.Test
ex3 = T.TestList
      [
        U.teq "ex30" (wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care") True
      , U.teq "ex31" (wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care") False
      , U.teq "ex32" (wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car") False
      , U.teq "ex33" (wordFitsTemplate "let" ['x','x'] "let") True
      ]
--------------------------------------------------------------------------------
--- Exercise 4.
-- |
-- produces all valid Scrabble words that match a given template using a
-- hand of available tiles. 
-- >>> wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l']
-- ["acre","bare","carb","care","carl","earl"]

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate temp hand = filter (wordFitsTemplate temp hand) allWords

ex4 :: T.Test
ex4 = T.TestList
      [
        U.teq "ex40" (wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l']) ["acre","bare","carb","care","carl","earl"]
      ]

--------------------------------------------------------------------------------
--- Exercise 5.
-- |
-- nction that gives the point value of any word.
-- >>> scrabbleValueWord "care"
-- 6
-- >>> scrabbleValueWord "quiz"
-- 22
scrabbleValueWord :: String -> Int
scrabbleValueWord = sum . map scrabbleValue

ex5 :: T.Test
ex5 = T.TestList
      [
        U.teq "ex50" (scrabbleValueWord "care") 6
      , U.teq "ex51" (scrabbleValueWord "quiz") 22
      ]

--------------------------------------------------------------------------------
--- Exercise 6.
-- |
-- >>> bestWords (wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l'])
-- ["carb"]
-- >>> bestWords ["cat", "rat", "bat"]
-- ["bat","cat"]
-- >>> bestWords []
-- []
bestWords :: [String] -> [String]
bestWords [] = []
bestWords ws = reverse
               $ map (\(a, _) ->a)
               $ head
               $ groupBy (\(_, a) (_, b) -> a == b)
               $ sortBy (\(_, x) (_, y) -> (flip compare) x y)
               $ zip ws (map scrabbleValueWord ws)

ex6 :: T.Test
ex6 = T.TestList
      [
        U.teq "ex60" (bestWords (wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l'])) ["carb"]
      , U.teq "ex61" (bestWords ["cat", "rat", "bat"]) ["bat","cat"]
      , U.teq "ex62" (bestWords []) []
        
      ]

--------------------------------------------------------------------------------
--- Exercise 7.
-- |
--
--
-- >>> scrabbleValueTemplate "?e??3" "peace"
-- 27
-- >>> scrabbleValueTemplate "De?2?" "peace"
-- 24
-- >>> scrabbleValueTemplate "??Tce" "peace"
-- 11

--scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate ts ws = sum $ map score $  zip ts ws 
  where
    score :: (Char, Char) -> Int
    score (t,c) = letterCoeff t * scrabbleValue c
    wordCoeff :: Int
    wordCoeff |elem '2' ts = 2
              |elem '3' ts = 3
              |otherwise = 1
    letterCoeff :: Char -> Int
    letterCoeff t |t == '?' = wordCoeff 
                  |t == 'D' = 2 * wordCoeff 
                  |t == 'T' = 3 * wordCoeff
                  |otherwise = wordCoeff

ex7 :: T.Test
ex7 = T.TestList
      [
        U.teq "ex70" (scrabbleValueTemplate "?e??3" "peace") 27
      , U.teq "ex71" (scrabbleValueTemplate "De?2?" "peace") 24
      , U.teq "ex72" (scrabbleValueTemplate "??Tce" "peace") 11
      ]

--------------------------------------------------------------------------------
--- Exercise 8.

--------------------------------------------------------------------------------
--- run all unit tests
hw2 :: IO T.Counts
hw2 = do
  T.runTestTT ex1
  T.runTestTT ex2
  T.runTestTT ex3
  T.runTestTT ex4
  T.runTestTT ex5
  T.runTestTT ex6
  T.runTestTT ex7  
  
  
