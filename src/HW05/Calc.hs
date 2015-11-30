{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module HW05.Calc where

import HW05.ExprT
import HW05.Parser

import qualified HW05.StackVM as S

-- Exercise 1 -----------------------------------------

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add exprA exprB) = eval exprA + eval exprB
eval (Mul exprA exprB) = eval exprA * eval exprB

-- Exercise 2 -----------------------------------------

evalStr :: String -> Maybe Integer
evalStr exprStr = case parseExp Lit Add Mul exprStr of
                       Nothing   -> Nothing
                       Just expr -> Just $ eval expr

-- Exercise 3 -----------------------------------------

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

-- Exercise 4 -----------------------------------------

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer
    deriving (Show, Eq)

instance Expr MinMax where
    lit = MinMax
    (MinMax x) `add` (MinMax y) = lit $ max x y
    (MinMax x) `mul` (MinMax y) = lit $ min x y

newtype Mod7 = Mod7 Integer
    deriving (Show, Eq)

instance Expr Mod7 where
    lit x = Mod7 $ x `mod` 7
    (Mod7 x) `add` (Mod7 y) = lit $ x + y
    (Mod7 x) `mul` (Mod7 y) = lit $ x * y

-- Exercise 5 -----------------------------------------

instance Expr S.Program where
    lit x = [S.PushI x]
    progA `add` progB = progA ++ progB ++ [S.Add]
    progA `mul` progB = progA ++ progB ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul
