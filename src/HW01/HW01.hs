{-# OPTIONS_GHC -Wall #-}
module HW01.HW01 where

-- Exercise 1 -----------------------------------------

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0    = []
    | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Exercise 2 -----------------------------------------

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse
    where doubleEveryOtherFromLeft [] = []
          doubleEveryOtherFromLeft [x] = [x]
          doubleEveryOtherFromLeft (x1:x2:xs) =
              x1 : 2 * x2 : doubleEveryOtherFromLeft xs

-- Exercise 3 -----------------------------------------

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- Exercise 4 -----------------------------------------

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- Exercise 5 -----------------------------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = let moveTop = hanoi (n - 1) a c b
                    moveTopBack = hanoi (n - 1) c b a
                in moveTop ++ [(a, b)] ++ moveTopBack
