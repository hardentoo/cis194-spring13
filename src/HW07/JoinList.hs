{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module HW07.JoinList where

import HW07.Buffer
import HW07.Scrabble
import HW07.Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

-- Exercise 1 -----------------------------------------

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jlA +++ jlB = Append m jlA jlB
  where
    m = tag jlA `mappend` tag jlB

-- Exercise 2 -----------------------------------------

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a)
    | i == 0    = Just a
    | otherwise = Nothing
indexJ i (Append _ jlA jlB)
    | i < sizeA = indexJ i jlA
    | otherwise = indexJ (i - sizeA) jlB
  where
    sizeA = getSize . size $ tag jlA

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0  = jl
dropJ _ Empty        = Empty
dropJ _ (Single _ _) = Empty
dropJ n (Append _ jlA jlB)
    | n < sizeA = let jlA' = dropJ n jlA
                      m = tag jlA' `mappend` tag jlB
                  in Append m jlA' jlB
    | otherwise = dropJ (n - sizeA) jlB
  where
    sizeA = getSize . size $ tag jlA

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0      = Empty
takeJ _ Empty           = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n (Append _ jlA jlB)
    | n > sizeA = let jlB' = takeJ (n - sizeA) jlB
                      m = tag jlA `mappend` tag jlB'
                  in Append m jlA jlB'
    | otherwise = takeJ n jlA
  where
    sizeA = getSize . size $ tag jlA

-- Exercise 3 -----------------------------------------

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4 -----------------------------------------

instance Buffer (JoinList (Score, Size) String) where
    toString Empty = ""
    toString (Single _ s) = s
    toString (Append _ jlA jlB) = let sA = toString jlA
                                      sB = toString jlB
                                      delim = if null sA || null sB
                                              then ""
                                              else "\n"
                                  in sA ++ delim ++ sB

    fromString = foldr1 (+++) . map toJoinList . lines
      where
        toJoinList s = Single (scoreString s, Size 1) s

    line = indexJ
    replaceLine n ls jl = takeJ n jl +++ fromString ls +++ dropJ (n+1) jl
    numLines = getSize . size . tag
    value = getScore . fst . tag
