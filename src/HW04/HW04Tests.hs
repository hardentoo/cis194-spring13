{-# OPTIONS_GHC -Wall #-}

module HW04.HW04Tests where

import HW04.HW04
import Testing

-- Exercise 1 -----------------------------------------

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

testFun1' :: [Integer] -> Bool
testFun1' xs = fun1 xs == fun1' xs

testFun2' :: Integer -> Bool
testFun2' n = fun2 n == fun2' n

ex01Tests :: [Test]
ex01Tests = [ Test "fun1' test" testFun1' [ [], [1..10], [1,3..10] ]
            , Test "fun2' test" testFun2' [ 1, 10, 27, 100, 4 ]
            ]

-- Exercise 2 -----------------------------------------

isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node _ left _ right) =
    let hLeft = getHeight left
        hRight = getHeight right
    in abs (hLeft - hRight) <= 1 && isBalanced left && isBalanced right

isConsistent :: Eq a => [a] -> Tree a -> Bool
isConsistent _ Leaf = True
isConsistent xs (Node h left x right) =
    x `elem` xs && h == max (getHeight left) (getHeight right) + 1 &&
    isConsistent xs left && isConsistent xs right

testFoldTree :: Eq a => [a] -> Bool
testFoldTree xs = isValid $ foldTree xs
  where
    isValid t = isBalanced t && isConsistent xs t

ex02Tests :: [Test]
ex02Tests = [ Test "foldTree test" testFoldTree [ "ABCDEFGHIJ" ] ]

-- Exercise 3 -----------------------------------------

testXor :: ([Bool], Bool) -> Bool
testXor (input, expected) = xor input == expected

ex03Tests :: [Test]
ex03Tests = [ Test "xor test" testXor
                [ ([False, True, False], True)
                , ([False, True, False, False, True], False)
                ]
            ]

-- Exercise 4 -----------------------------------------

testSieveSundaram :: (Integer, [Integer]) -> Bool
testSieveSundaram (n, expected) = sieveSundaram n == expected

ex04Tests :: [Test]
ex04Tests = [ Test "sieveSundaram test" testSieveSundaram
                [ (0, [])
                , (1, [3])
                , (5, [3, 5, 7, 11])
                , (20, [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41])
                ]
            ]

-- All tests ------------------------------------------

allTests :: [Test]
allTests = ex01Tests ++ ex02Tests ++ ex03Tests ++ ex04Tests
