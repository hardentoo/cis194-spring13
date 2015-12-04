{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW07.Scrabble where

import Data.Char (toUpper)

newtype Score = Score { getScore :: Int }
    deriving (Eq, Show, Ord, Num)

instance Monoid Score where
    mempty = Score 0
    (Score x) `mappend` (Score y) = Score (x + y)

score :: Char -> Score
score c
    | toUpper c `elem` "AEILNORSTU" = 1
    | toUpper c `elem` "DG"         = 2
    | toUpper c `elem` "BCMP"       = 3
    | toUpper c `elem` "FHVWY"      = 4
    | toUpper c `elem` "K"          = 5
    | toUpper c `elem` "JX"         = 8
    | toUpper c `elem` "QZ"         = 10
    | otherwise                     = 0

scoreString :: String -> Score
scoreString = sum . map score
