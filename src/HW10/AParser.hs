{- CIS 194 HW 10
   due Monday, 1 April
-}

module HW10.AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1 -----------------------------------------

instance Functor Parser where
    f `fmap` (Parser p) = Parser $ \s ->
        case p s of
            Nothing     -> Nothing
            Just (a, c) -> Just (f a, c)

-- Exercise 2 -----------------------------------------

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    (Parser pA) <*> pB = Parser $ \s ->
        case pA s of
            Nothing      -> Nothing
            Just (f, s') -> runParser (f <$> pB) s'

-- Exercise 3 -----------------------------------------

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = Parser $ \s ->
    case runParser abParser s of
        Nothing     -> Nothing
        Just (_, r) -> Just ((), r)

intPair :: Parser [Integer]
intPair = makePair <$> posInt <*> nextPosInt
  where
    makePair x y = [x, y]
    nextPosInt = Parser $ \s ->
        if null s || head s /= ' '
        then Nothing
        else runParser posInt $ tail s

-- Exercise 4 -----------------------------------------

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser pA) <|> (Parser pB) = Parser $ \s -> pA s <|> pB s

-- Exercise 5 -----------------------------------------

intOrUppercase :: Parser ()
intOrUppercase = parseInt <|> parseUppercase
  where
    parseInt = Parser $ \s ->
        case runParser posInt s of
            Nothing     -> Nothing
            Just (_, s') -> Just ((), s')
    parseUppercase = Parser $ \s ->
        case runParser (satisfy isUpper) s of
            Nothing     -> Nothing
            Just (_, s') -> Just ((), s')
