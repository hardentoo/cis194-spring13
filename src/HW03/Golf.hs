{-# OPTIONS_GHC -Wall #-}

module HW03.Golf where

import qualified Data.List as L

-- Exercise 1 -----------------------------------------

skips :: [a] -> [[a]]
skips xs = doSkip 1
  where doSkip n
            | n > length xs = []
            | otherwise = let els = filter (\(_,i) -> i `mod` n == 0) $ zip xs [1..]
                          in map fst els : doSkip (n + 1)

-- Exercise 2 -----------------------------------------

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima xs@(x:y:z:_) = if x < y && y > z
                           then y : localMaxima (tail xs)
                           else localMaxima $ tail xs

-- Exercise 3 -----------------------------------------

histogram :: [Integer] -> String
histogram xs =
    let table = map count [0..9]
        hist = genHist table
    in L.intercalate "\n" $ reverse hist ++ ["==========", "0123456789"]
  where
    count x = length $ filter (== x) xs
    genHist t
        | all (<= 0) t = []
        | otherwise =
            map (\c -> if c > 0 then '*' else ' ') t : genHist (map (subtract 1) t)

