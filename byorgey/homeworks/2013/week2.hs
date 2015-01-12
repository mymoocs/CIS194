{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage = parseLine . words

parse :: String -> [LogMessage]
parse str = map parseMessage (lines str)

parseLine ::[String] -> LogMessage
parseLine ("I":t:xs) = LogMessage Info (read t) (unwords xs)
parseLine ("W":t:xs) = LogMessage Warning (read t) (unwords xs)
parseLine ("E":s:t:xs) = LogMessage (Error (read s)) (read t) (unwords xs)
parseLine xs  =  Unknown (unwords xs)


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert (msg@(LogMessage _ t _)) (Node left (root@(LogMessage _ t1 _)) right) 
                |t > t1 = Node left root (insert msg right)              
                |otherwise =Node (insert msg left) root right

build :: [LogMessage] -> MessageTree
build xs = build' xs Leaf
    
build' ::[LogMessage] -> MessageTree -> MessageTree
build' [] t = t
build' (x:xs) t = build' xs (insert x t)              


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf  = []
inOrder (Node left msg right)  = inOrder left ++ [msg] ++ inOrder right 




testBuild = build fortest


fortest = [LogMessage Info 5053 "pci_id: con ing!",
           LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",
           LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled",
           LogMessage Info 4076 "verse.'",
           LogMessage Info 4764 "He trusts to you to set them free,",
           LogMessage Info 858 "your pocket?' he went on, turning to Alice.",
           LogMessage Info 898 "would be offended again.",
           LogMessage Info 3753 "pci 0x18fff steresocared, overne: "]


{--ff = do
  list <- testParse parse 10 "error.log"
  --let g = map insert list
  print list--}

