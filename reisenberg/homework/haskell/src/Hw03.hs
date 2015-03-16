{-# OPTIONS_GHC -Wall #-}


module Hw03 where

import Hw03Log
import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U
import Data.List (sortBy, isInfixOf)
import Data.Char (toLower)
-- import System.IO (readFile)
-- import Control.Monad ((=<<))

--------------------------------------------------------------------------------
--- Exercise 1.
-- |
--
-- >>> parseMessage "E 2 562 help help"
-- ValidLM (LogMessage (Error 2) 562 "help help")
-- >>> parseMessage "I 29 la la la"
-- ValidLM (LogMessage Info 29 "la la la")
-- >>> parseMessage "This is not in the right format"
-- InvalidLM "This is not in the right format"

parseMessage :: String -> MaybeLogMessage  
parseMessage str =
  case (words str) of
   ("E":n:t:ls) -> case (readInt n) of
                    InvalidInt -> InvalidLM str
                    ValidInt i -> case (readInt t) of
                                   InvalidInt -> InvalidLM str
                                   ValidInt k -> ValidLM (LogMessage (Error i) k (unwords ls))
   ("I":t:ls) -> case (readInt t) of
                   InvalidInt -> InvalidLM str
                   ValidInt i -> ValidLM (LogMessage Info i (unwords ls))
   ("W":t:ls) -> case (readInt t) of
                   InvalidInt -> InvalidLM str
                   ValidInt i -> ValidLM (LogMessage Warning i (unwords ls))

   _           -> InvalidLM str
   

ex1 :: T.Test
ex1 = T.TestList
  [
    U.teq "ex10" (parseMessage "E 2 562 help help") (ValidLM (LogMessage (Error 2) 562 "help help"))
  , U.teq "ex11" (parseMessage "I 29 la la la") (ValidLM (LogMessage Info 29 "la la la"))
  , U.teq "ex12" (parseMessage "This is not in the right format") (InvalidLM "This is not in the right format")
  ]

--------------------------------------------------------------------------------
--- Exercise 2.
-- |
-- throws out invalid messages.
-- >>>validMessagesOnly $(parseMessage "E 2 562 help help"):(parseMessage "I 29 la la la"):(parseMessage "This is not in the right format"):[]
-- [LogMessage (Error 2) 562 "help help",LogMessage Info 29 "la la la"]

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly (m:ms) = case m of
                            ValidLM l -> l:validMessagesOnly ms
                            _         -> validMessagesOnly ms

ex2 :: T.Test
ex2 = T.TestList
      [
        U.teq "ex20" (validMessagesOnly ms) [LogMessage (Error 2) 562 "help help",LogMessage Info 29 "la la la"]
      ]
      where
        ms = (parseMessage "E 2 562 help help"):(parseMessage "I 29 la la la"):(parseMessage "This is not in the right format"):[]

--------------------------------------------------------------------------------
--- Exercise 3.
-- |
-- function which parses an entire log file at once and returns
-- its contents as a list of LogMessages.
-- for testing run
-- >>>testParse parse 3 "error.log"
-- LogMessage Info 5053 "pci_id: con ing!"
-- LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)"
-- LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"        
        
parse :: String -> [LogMessage]        
parse = validMessagesOnly . map parseMessage . lines
  
testParse3 = testParse parse 10 "src/error.log"   


--------------------------------------------------------------------------------
--- Exercise 4.
-- |
-- >>>let msg1 = LogMessage Warning 153 "Not a speck of light is showing, so the danger must be growing..."
-- >>>let msg2 = LogMessage Info 208 "the Weighted Companion Cube cannot talk"
-- >>>compareMsgs msg1 msg2
-- LT
-- >>>let msg3 = LogMessage (Error 101) 2001 "My God! It's full of stars!"
-- >>>let msg4 = LogMessage Info 2001 "Daisy, Daisy, give me your answer do."
-- >>>compareMsgs msg3 msg4
-- EQ

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ t1 _) (LogMessage _ t2 _) = compare t1 t2


ex4 :: T.Test
ex4 = T.TestList
      [
        U.teq "ex40" (compareMsgs msg1 msg2) LT
      , U.teq "ex41" (compareMsgs msg3 msg4) EQ
      ]
      where
        msg1 = LogMessage Warning 153 "Not a speck of light is showing, so the danger must be growing..."
        msg2 = LogMessage Info 208 "the Weighted Companion Cube cannot talk"
        msg3 = LogMessage (Error 101) 2001 "My God! It's full of stars!"
        msg4 = LogMessage Info 2001 "Daisy, Daisy, give me your answer do."

--------------------------------------------------------------------------------
--- Exercise 5.
-- |
-- function that sort the list.
-- >>>sortMessages [LogMessage (Error 2) 562 "help help",LogMessage Info 29 "la la la"]
-- [LogMessage Info 29 "la la la",LogMessage (Error 2) 562 "help help"]

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages  = sortBy compareMsgs

ex5 :: T.Test
ex5 = T.TestList
      [
        U.teq "ex50" (sortMessages [LogMessage (Error 2) 562 "help help",LogMessage Info 29 "la la la"]) [LogMessage Info 29 "la la la",LogMessage (Error 2) 562 "help help"] 
      ]


--------------------------------------------------------------------------------
--- Exercise 6.
-- |
-- eturns a list of the messages corresponding to any errors with a severity of
-- 50 or greater, sorted by timestamp
-- >>>take 3 testWhatWentWrong parse whatWentWrong "error.log"
-- Mustardwatch opened, please close for proper functioning!
-- All backup mustardwatches are busy
-- Depletion of mustard stores detected!

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =  map getMsg .  sortBy compareMsgs . filter isError
  where
    getMsg (LogMessage _ s msg) = msg -- ++ " " ++  (show s)
    isError (LogMessage (Error s) _ msg) |s >= 50 = True
                                         |otherwise = False
    isError _ = False



ex6 :: T.Test
ex6 = T.TestList
      [
        -- U.teq "ex60" (take 1 $ testWhatWentWrong parse whatWentWrong "error.log") "Mustardwatch opened, please close for proper functioning!"
      ]


--------------------------------------------------------------------------------
--- Exercise 7.
-- |
-- that filters a list of LogMessages to include only those messages that
-- contain the string provided., match case-insensitive

messagesAbout :: String -> [String] -> [String]
messagesAbout pattern  = filter p
  where
    p str = isInfixOf (stringToLower pattern) (stringToLower str)
    p _
      = False

    stringToLower = map toLower

--------------------------------------------------------------------------------
--- Exercise 8.
-- |
--
stringToLower :: String -> String
stringToLower = map toLower

isContains :: String -> LogMessage -> Bool
isContains pattern (LogMessage _ _ str) = isInfixOf (stringToLower pattern) (stringToLower str)

isError :: LogMessage -> Bool
isError (LogMessage (Error s) _ msg) |s >= 50 = True
                                     |otherwise = False
isError _ = False

getMsg :: LogMessage -> String
getMsg (LogMessage _ s msg) = msg


(|||) :: (LogMessage -> Bool) -> (LogMessage -> Bool) -> LogMessage -> Bool
(|||) f g x = f x || g x -- (||) is Haskell’s ordinary "or" opera


whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced pattern =  map getMsg . sortBy compareMsgs . filter (isError ||| (isContains pattern))


-- for test run
-- testWhatWentWrong parse (whatWentWrongEnhanced "fail") "src/error.log")

--------------------------------------------------------------------------------
--- Test List
        
hw3 :: IO T.Counts
hw3 = do
  T.runTestTT ex1
  T.runTestTT ex2
  T.runTestTT ex4
  T.runTestTT ex5
  T.runTestTT ex6

