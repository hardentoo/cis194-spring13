{-# OPTIONS_GHC -Wall #-}

module HW02.LogAnalysis where

import HW02.Log

-- Exercise 1 -----------------------------------------

parseMessage :: String -> LogMessage
parseMessage message@(code:_)
    | code == 'I' = let (_:timeStampStr:msgWords) = words message
                        timeStamp = read timeStampStr
                    in LogMessage Info timeStamp $ unwords msgWords

    | code == 'W' = let (_:timeStampStr:msgWords) = words message
                        timeStamp = read timeStampStr
                    in LogMessage Warning timeStamp $ unwords msgWords

    | code == 'E' = let (_:errLevelStr:timeStampStr:msgWords) = words message
                        errLevel = read errLevelStr
                        timeStamp = read timeStampStr
                    in LogMessage (Error errLevel) timeStamp (unwords msgWords)

parseMessage message = Unknown message

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2 -----------------------------------------

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage (Node leftTree message rightTree) =
    let (LogMessage _ logTimeStamp _) = logMessage
        (LogMessage _ timeStamp _) = message
    in
        if logTimeStamp < timeStamp
        then Node (insert logMessage leftTree) message rightTree
        else Node leftTree message (insert logMessage rightTree)

-- Exercise 3 -----------------------------------------

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4 -----------------------------------------

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree message rightTree) =
    inOrder leftTree ++ [message] ++ inOrder rightTree

-- Exercise 5 -----------------------------------------

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter isRelevant
  where isRelevant (LogMessage (Error level) _ _) = level >= 50
        isRelevant _ = False
        getMessage (Unknown message) = message
        getMessage (LogMessage _ _ message) = message
