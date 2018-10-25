module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = let wordList = words str in
  case wordList of
    ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg) 
    ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
    ("E":lvl:ts:msg) -> LogMessage (Error (read lvl)) (read ts) (unwords msg)
    _ -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@LogMessage{} Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ x _) (Node left msg2@(LogMessage _ y _) right)
  | x > y = Node left msg2 (insert msg1 right)
  | otherwise = Node (insert msg1 left) msg2 right
insert _ tree = tree 

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- 将排好的树中序遍历出来
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) =
  inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extract . inOrder . build . filter (server 50)

extract :: LogMessage -> String
extract (LogMessage _ _ msg) = msg
extract _ = ""

server :: Int -> LogMessage -> Bool
server val (LogMessage (Error level) _ _)
 | val > level = True
 | otherwise = False
server _ _ = False
