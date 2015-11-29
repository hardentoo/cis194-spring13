{-# OPTIONS_GHC -Wall #-}

module HW02.HW02Tests where

import HW02.Log
import HW02.LogAnalysis
import Testing

-- Exercise 1 -----------------------------------------

testParseMessage :: (String, LogMessage) -> Bool
testParseMessage (msg, logMsg) = parseMessage msg == logMsg

ex01Tests :: [Test]
ex01Tests = [ Test "parseMessage test" testParseMessage
                [ ( "E 2 562 help help", LogMessage (Error 2) 562 "help help" )
                , ( "I 29 la la la", LogMessage Info 29 "la la la" )
                , ( "This is not in the right format"
                  , Unknown "This is not in the right format"
                  )
                ]
            ]
