{-# OPTIONS_GHC -Wall #-}

module HW03.HW03Tests where

import HW03.Golf
import Testing

-- Exercise 1 -----------------------------------------

testSkips :: Eq a => ([a], [[a]]) -> Bool
testSkips (xs, expected) = skips xs == expected

ex01Tests :: [Test]
ex01Tests = [ Test "skips test" testSkips
                [ ("ABCD", ["ABCD", "BD", "C", "D"])
                , ("hello!", ["hello!", "el!", "l!", "l", "o", "!"])
                , ("1", ["1"])
                , ([], [])
                ]
            ]

-- Exercise 2 -----------------------------------------

testLocalMaxima :: ([Integer], [Integer]) -> Bool
testLocalMaxima (xs, expected) = localMaxima xs == expected

ex02Tests :: [Test]
ex02Tests = [ Test "localMaxima test" testLocalMaxima
                [ ([2, 9, 5, 6, 1], [9, 6])
                , ([2, 3, 4, 1, 5], [4])
                , ([1, 2, 3, 4, 5], [])
                , ([], [])
                , ([1], [])
                , ([1, 2], [])
                ]
            ]

-- Exercise 3 -----------------------------------------

testHistogram :: ([Integer], String) -> Bool
testHistogram (xs, expected) = histogram xs == expected

ex03Tests :: [Test]
ex03Tests = [ Test "histogram test" testHistogram
                [ ( [1, 1, 1, 5]
                  , " *        \n *        \n *   *    \n==========\n0123456789"
                  )
                , ( [1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9]
                  , "    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789"
                  )
                ]
            ]
