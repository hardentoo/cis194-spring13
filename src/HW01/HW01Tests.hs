-- CIS 194, Spring 2013
--
-- Test cases for HW 01

module HW01.HW01Tests where

import HW01.HW01
import Testing

-- Exercise 1 -----------------------------------------

testToDigits :: (Integer, [Integer]) -> Bool
testToDigits (n, digits) = toDigits n == digits

testToDigitsRev :: (Integer, [Integer]) -> Bool
testToDigitsRev (n, digits) = toDigitsRev n == digits

ex01Tests :: [Test]
ex01Tests = [ Test "toDigits test" testToDigits
                [ (1234, [1, 2, 3, 4])
                , (0, [])
                , (-17, [])
                ]
            , Test "toDigitsRev test" testToDigitsRev
                [ (1234, [4, 3, 2, 1])
                , (0, [])
                , (-17, [])
                ]
            ]

-- Exercise 2 -----------------------------------------

testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (input, expected) = doubleEveryOther input == expected

ex02Tests :: [Test]
ex02Tests = [ Test "doubleEveryOther test" testDoubleEveryOther
                [ ([8, 7, 6, 5], [16, 7, 12, 5])
                , ([1, 2, 3], [1, 4, 3])
                ]
            ]

-- Exercise 3 -----------------------------------------

testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (xs, expected) = sumDigits xs == expected

ex03Tests :: [Test]
ex03Tests = [ Test "sumDigits test" testSumDigits
                [ ([16, 7, 12, 5], 22)
                , ([], 0)
                ]
            ]

-- Exercise 4 -----------------------------------------

testValidate :: (Integer, Bool) -> Bool
testValidate (n, expected) = validate n == expected

ex04Tests :: [Test]
ex04Tests = [ Test "validate test" testValidate
                [ (4012888888881881, True)
                , (4012888888881882, False)
                ]
            ]

-- Exercise 5 -----------------------------------------

testHanoi :: (Integer, [Move]) -> Bool
testHanoi (n, expected) = hanoi n "a" "b" "c" == expected

ex05Tests :: [Test]
ex05Tests = [ Test "hanoi test" testHanoi
                [ (2, [ ("a", "c"), ("a", "b"), ("c", "b") ])
                , (3, [ ("a", "b")
                      , ("a", "c")
                      , ("b", "c")
                      , ("a", "b")
                      , ("c", "a")
                      , ("c", "b")
                      , ("a", "b")
                      ])
                ]
            ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = ex01Tests ++ ex02Tests ++ ex03Tests ++ ex04Tests ++ ex05Tests
