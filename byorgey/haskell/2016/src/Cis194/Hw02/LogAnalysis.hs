-- CIS 194 Homework 2

module Cis194.Hw02.LogAnalysis where

import Cis194.Hw02.Log

-------------------------------------------------------------------------------
-- Exercise 1


-- | @parseMessage s@ parsing a text @line@ to @LogMessage@ type.
--
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
--
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
--
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage line =
    case  words line of
      ("E":n:t:r) -> LogMessage (Error (read n)) (read t) (unwords r)
      ("I":t:r)   -> LogMessage Info (read t) (unwords r)
      ("W":t:r)   -> LogMessage Warning (read t) (unwords r)
      _           -> Unknown "This is not in the right format"


-- | @parse@ whole text line by line.

parse :: String -> [LogMessage]
parse = map parseMessage .  lines

-------------------------------------------------------------------------------
-- Exercise 2
--data MessageTree = Leaf
  --               | Node MessageTree LogMessage MessageTree

insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert (Unknown _) t = t
insert m1@(LogMessage _ t1 _) (Node l m2@(LogMessage _ t2 _) r)
    | t1 > t2 =  Node l m2 (insert m1 r)
    | otherwise =  Node (insert m1 l) m2 r


-- |
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)



inOrder :: MessageTree -> [LogMessage]
inOrder = undefined
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = undefined
