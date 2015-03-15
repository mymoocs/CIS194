{-# OPTIONS_GHC -Wall #-}


module Hw03 where

import Hw03Log
import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

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
   ("E":n:t:log) -> case (readInt n) of
                    InvalidInt -> InvalidLM str
                    ValidInt i -> case (readInt t) of
                                   InvalidInt -> InvalidLM str
                                   ValidInt t -> ValidLM (LogMessage (Error i) t (unwords log))
   ("I":t:log) -> case (readInt t) of
                   InvalidInt -> InvalidLM str
                   ValidInt t -> ValidLM (LogMessage Info t (unwords log))
   _           -> InvalidLM str                                 

ex1 :: T.Test
ex1 = T.TestList
  [
    U.teq "ex10" (parseMessage "E 2 562 help help") (ValidLM (LogMessage (Error 2) 562 "help help"))
  , U.teq "ex11" (parseMessage "I 29 la la la") (ValidLM (LogMessage Info 29 "la la la"))
  , U.teq "ex12" (parseMessage "This is not in the right format") (InvalidLM "This is not in the right format")
  ]

hw3 :: IO T.Counts
hw3 = do
  T.runTestTT ex1
