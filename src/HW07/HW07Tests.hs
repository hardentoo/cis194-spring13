{-# OPTIONS_GHC -Wall #-}

module HW07.HW07Tests where

import Data.Monoid (Sum(..))

import HW07.JoinList
import HW07.Scrabble
import HW07.Sized
import Testing

-- Exercise 1 -----------------------------------------

testTriplePlus :: Eq a
               => ( JoinList (Sum Integer) a
                  , JoinList (Sum Integer) a
                  , JoinList (Sum Integer) a
                  )
               -> Bool
testTriplePlus (jlA, jlB, jlC) = jlA +++ jlB == jlC

ex01Tests :: [Test]
ex01Tests = [ Test "+++ test" testTriplePlus
                [ ( Empty
                  , Empty
                  , Append (Sum 0) Empty Empty
                  )
                , ( Empty
                  , Single (Sum 4) "a"
                  , Append (Sum 4) Empty (Single (Sum 4) "a")
                  )
                , ( Single (Sum 4) "a"
                  , Empty
                  , Append (Sum 4) (Single (Sum 4) "a") Empty
                  )
                , ( Single (Sum 5) "a"
                  , Single (Sum 3) "b"
                  , Append (Sum 8) (Single (Sum 5) "a") (Single (Sum 3) "b")
                  )
                ]
            ]

-- Exercise 2 -----------------------------------------

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

testIndexJ :: Eq a => (Int, JoinList Size a) -> Bool
testIndexJ (ix, jl) = indexJ ix jl == (jlToList jl !!? ix)
  where
    []     !!? _         = Nothing
    _      !!? i | i < 0 = Nothing
    (x:_) !!? 0          = Just x
    (_:xs) !!? i         = xs !!? (i-1)

testDropJ :: Eq a => (Int, JoinList Size a) -> Bool
testDropJ (n, jl) = jlToList (dropJ n jl) == drop n (jlToList jl)

testTakeJ :: Eq a => (Int, JoinList Size a) -> Bool
testTakeJ (n, jl) = jlToList (takeJ n jl) == take n (jlToList jl)

ex02Tests :: [Test]
ex02Tests = [ Test "indexJ test" testIndexJ testCases
            , Test "dropJ test" testDropJ testCases
            , Test "takeJ test" testTakeJ testCases
            ]
  where
    testCases = [ ( 5, Empty )
                , ( 10, Single (Size 1) 'a' )
                , ( -4, Single (Size 1) 'a' )
                , ( 1
                  , Append (Size 2)
                        (Single (Size 1) 'a')
                        (Single (Size 1) 'b')
                  )
                , ( 2
                  , Append (Size 4)
                        (Append (Size 2)
                            (Single (Size 1) 'a')
                            (Single (Size 1) 'b'))
                        (Append (Size 2)
                            (Single (Size 1) 'c')
                            (Single (Size 1) 'd'))
                  )
                ]

-- Exercise 3 -----------------------------------------

scores :: [Score]
scores = [1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3, 1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10]

testScore :: () -> Bool
testScore _ = and $ zipWith check (['a'..'z'] ++ ['A'..'Z']) (scores ++ scores)
  where
    check c s = score c == s

testScoreString :: (String, Score) -> Bool
testScoreString (s, n) = scoreString s == n

testScoreLine :: (String, JoinList Score String) -> Bool
testScoreLine (s, jl) = scoreLine s == jl

ex03Tests :: [Test]
ex03Tests = [ Test "score test" testScore [()]
            , Test "scoreString test" testScoreString
                [ ("yay ", Score 9)
                , ("haskell!", Score 14)
                ]
            , Test "scoreLine test" testScoreLine
                [ ("yay ", Single (Score 9) "yay ")
                , ("haskell!", Single (Score 14) "haskell!")
                ]
            ]

-- All tests ------------------------------------------

allTests :: [Test]
allTests = ex01Tests ++ ex02Tests ++ ex03Tests
