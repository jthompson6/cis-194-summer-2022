module Homework.Week02.Assignment (
  build,
  inOrder,
  insert,
  parse,
  parseMessage,
  whatWentWrong,
  LogMessage(..),
  MessageTree(..),
  MessageType(..),
  TimeStamp
) where

import Homework.Week02.Log

-- #1a
parseMessageOfWords :: [String] -> LogMessage
-- parseMessageOfWords line
parseMessageOfWords ("W": (timestamp: remainder)) = LogMessage Warning (read timestamp ::Int) (unwords remainder)
parseMessageOfWords ("E": (severity: (timestamp: remainder))) = LogMessage (Error (read severity ::Int)) (read timestamp ::Int) (unwords remainder)
parseMessageOfWords ("I": (timestamp: remainder)) = LogMessage Info (read timestamp ::Int) (unwords remainder)
parseMessageOfWords line = Unknown (unwords line)

parseMessage :: String -> LogMessage
parseMessage line = parseMessageOfWords (words line)
-- parseMessage line
--   case ws of
--       "E": (severity: (timestamp: remainder)) -> LogMessage (Error (decimalStringToInt severity)) (decimalStringToInt (timstamp)) remainder
--       "W": (timestamp: remainder) -> LogMessage Warning (decimalStringToInt (timstamp)) remainder
--       "I": (timestamp: remainder) -> LogMessage Info (decimalStringToInt (timstamp)) remainder
--       line -> Unknown line
--   where ws = words line


-- #1b
parse :: String -> [LogMessage]
parse logContent = map parseMessage (lines logContent)

-- #2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown x) tree = tree
insert message Leaf = Node Leaf message Leaf
insert l1@(LogMessage _ l1Timestamp _) (Node leftTree l2@(LogMessage _ l2Timestamp _) rightTree) = if l2Timestamp > l1Timestamp then Node (insert l1 leftTree) l2 rightTree else Node leftTree l2 (insert l1 rightTree)

-- #3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:[]) = insert x Leaf
build (x:ys) = insert x (build ys)


-- #4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree message rightTree) = inOrder leftTree ++ (message : (inOrder rightTree))

-- #5
isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error severity) _ _) = if severity > 49 then True else False
isSevere _ = False

getContent :: LogMessage -> String
getContent (LogMessage _ _ content) = content

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map getContent (inOrder (build (filter isSevere messages)))
