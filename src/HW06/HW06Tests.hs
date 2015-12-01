{-# OPTIONS_GHC -Wall #-}

module HW06.HW06Tests where

import HW06.HW06
import Testing

-- Exercise 1 -----------------------------------------

testFib :: () -> Bool
testFib _ = map fib [0..7] == [0, 1, 1, 2, 3, 5, 8, 13]

testFibs1 :: () -> Bool
testFibs1 _ = take 8 fibs1 == [0, 1, 1, 2, 3, 5, 8, 13]

ex01Tests :: [Test]
ex01Tests = [ Test "fib test" testFib [()]
            , Test "fibs1 test" testFibs1 [()]
            ]

-- Exercise 2 -----------------------------------------

testFibs2 :: () -> Bool
testFibs2 _ = take 8 fibs2 == [0, 1, 1, 2, 3, 5, 8, 13]

ex02Tests :: [Test]
ex02Tests = [ Test "fibs2 test" testFibs2 [()] ]

-- Exercise 5 -----------------------------------------

testNats :: () -> Bool
testNats _ = take 50 (streamToList nats) == [0..49]

testRuler :: () -> Bool
testRuler _ = take 16 (streamToList ruler) == expected
  where
    expected = [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4]

ex05Tests :: [Test]
ex05Tests = [ Test "nats test" testNats [()] 
            , Test "ruler test" testRuler [()]
            ]

-- Exercise 6 -----------------------------------------

testX :: () -> Bool
testX _ = take 5 (streamToList x) == [0, 1, 0, 0, 0]

testFibs3 :: () -> Bool
testFibs3 _ = take 8 (streamToList fibs3) == [0, 1, 1, 2, 3, 5, 8, 13]

ex06Tests :: [Test]
ex06Tests = [ Test "x test" testX [()]
            , Test "fibs3 test" testFibs3 [()]
            ]

-- Exercise 7 -----------------------------------------

testFib4 :: () -> Bool
testFib4 _ = map fib4 [0..7] == [0, 1, 1, 2, 3, 5, 8, 13]

ex07Tests :: [Test]
ex07Tests = [ Test "fib4 test" testFib4 [()] ]

-- All tests ------------------------------------------

allTests :: [Test]
allTests = ex01Tests ++ ex02Tests ++ ex05Tests ++ ex06Tests ++ ex07Tests
