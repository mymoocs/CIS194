{-# OPTIONS_GHC -Wall #-}
module Cis194.Hw02_LogAnalysis
    ( parseMessage
    , parse
    , insert
    , build
    , inOrder
    , whatWentWrong
    )
    where
      
import Cis194.Log
    ( LogMessage(..)
    , MessageType(..)
    , MessageTree(..)  
    )

import Data.List (sortBy)    
------------------------------------------------------------------------------
-- Exercise 1

-- | @parseMessage l@ parse the log message line @l@
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
--
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
--
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"

parseMessage :: String
             -> LogMessage
parseMessage e =
    case words e of
      ("E":n:t:xs) -> LogMessage (Error (read n)) (read t) $ unwords xs
      ("I":n:xs)   -> LogMessage Info (read n) $ unwords xs
      ("W":n:xs)   -> LogMessage Warning (read n) $ unwords xs                      
      _            -> Unknown e


-- | @parse content@ parse the pease of log text @content@ 
parse :: String -> [LogMessage]
parse = map parseMessage .  lines


------------------------------------------------------------------------------
-- Exercise 2

-- | @insert logmsg msgtree@ insert a new @logmsg@ into sorted @msgtree@, prodcusing
--   a new sorted MessageTree.
--   if insert is given a LogMessage which is Unknown, it should return the MessageTree
--   unchanged.


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree          = tree
insert _  (Node _ (Unknown _) _) = error "impossible, for type checker press warning"
                                                                           
insert msg Leaf                  = Node Leaf msg Leaf                          
insert m@(LogMessage _ t _) (Node l m1@(LogMessage _ t1 _)  r)
    | t > t1    = Node l m1 (insert m r)
    | otherwise = Node (insert m l) m1 r

------------------------------------------------------------------------------
-- Exercise 3

-- | @build ls@ which builds up a MessageTree containing the messages in the @ls@ list,
--   by successively inserting the messages into a MessageTree (beginning with a Leaf).

build :: [LogMessage] -> MessageTree
build  = foldr insert Leaf

------------------------------------------------------------------------------
-- Exercise 4

-- | @inOrder tree@  which takes a sorted @tree@  MessageTree and produces a list of all
--   the LogMessages it contains, sorted by timestamp from smallest to biggest.
--   (This is known as an in-order traversal of the MessageTree.)
--   e.g. inOrder (build tree)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node Leaf m Leaf) = [m]
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

------------------------------------------------------------------------------
-- Exercise 5

-- | @whatWentWrong ls@  which takes an unsorted list of LogMessages @ls@, and returns
--   a list of the messages corresponding to any errors with a severity of 50 or greater,
--   sorted by timestamp.

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map msg . sortBy sortMsg . getErrors
    where
      msg (LogMessage _ _ s) = s
      msg _ = ""
      getErrors [] = []                 
      getErrors (m@(LogMessage (Error n) _ _):ls)
          | n > 50 =  m:getErrors ls
          | otherwise = getErrors ls
      getErrors (_:ls) = getErrors ls

sortMsg :: LogMessage -> LogMessage -> Ordering
sortMsg (LogMessage _ t1 _) (LogMessage _ t2 _) = compare t1 t2
sortMsg _ _ = EQ
