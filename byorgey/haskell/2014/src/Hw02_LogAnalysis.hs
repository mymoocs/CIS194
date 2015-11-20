-- CIS 194 Homework 2

{-# OPTIONS_GHC -Wall #-}


module Hw02_LogAnalysis where

import Log

-- 1.
parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
                  ("I":t:xs) -> LogMessage Info (read t) $ unwords xs
                  ("E":n:t:xs) -> LogMessage (Error (read n)) (read t) $ unwords xs
                  _ -> Unknown s


parse :: String -> [LogMessage]
parse  = map parseMessage . lines

-- 2.

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _ ) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ t1 _) (Node l m@(LogMessage _ t2 _) r )
  | t1 < t2 = Node (insert msg l) m r
  | otherwise = Node l m (insert  msg r)

-- insert (LogMessage _ _ _) (Node _ (Unknown _) _)

-- 3.
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m (build ms)
  
-- 4.

inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
                Leaf -> []
                (Node l m r) -> (inOrder l) ++ [m] ++ (inOrder r)


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong  =
  (map (\(LogMessage _ _ s)-> s)) . (filter errorpred ) . inOrder . build . errors
    where
      errors [] = []
      errors (m@(LogMessage (Error _) _ _):xs) = m : errors xs
      errors (_:xs) = errors xs
      errorpred (LogMessage (Error s) _  _) = s > 50
      errorpred _ = False
                                    
