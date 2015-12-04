module Main where

import HW07.Editor
import HW07.JoinList
import HW07.Scrabble
import HW07.Sized

main = runEditor editor (Single (Score 0, Size 0) "")
