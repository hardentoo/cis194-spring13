{-# OPTIONS_GHC -Wall #-}

module HW05.HW05Tests where

import HW05.Calc
import HW05.ExprT
import Testing

-- Exercise 1 -----------------------------------------

testEval :: (ExprT, Integer) -> Bool
testEval (e, expected) = eval e == expected

ex01Tests :: [Test]
ex01Tests = [ Test "eval test" testEval
                [ (Lit 5, 5)
                , (Add (Lit 10) (Lit 12), 22)
                , (Mul (Add (Lit 2) (Lit 3)) (Lit 4), 20)
                ]
            ]

-- Exercise 2 -----------------------------------------

testEvalStr :: (String, Maybe Integer) -> Bool
testEvalStr (e, expected) = evalStr e == expected

ex02Tests :: [Test]
ex02Tests = [ Test "evalStr test" testEvalStr
                [ ("2", Just 2)
                , ("7+10", Just 17)
                , ("(2+3)*4", Just 20)
                , ("2+3*4", Just 14)
                , ("2+3*", Nothing)
                ]
            ]

-- Exercise 3 -----------------------------------------

testLit :: (Eq a, Expr a) => (Integer, a) -> Bool
testLit (n, e) = lit n == e

testAdd :: (Eq a, Expr a) => (a, a, a) -> Bool
testAdd (exprA, exprB, exprC) = add exprA exprB == exprC

testMul :: (Eq a, Expr a) => (a, a, a) -> Bool
testMul (exprA, exprB, exprC) = mul exprA exprB == exprC

ex03Tests :: [Test]
ex03Tests = [ Test "ExprT lit test" testLit
                [ (5, Lit 5)
                , (0, Lit 0)
                ]
            , Test "ExprT add test" testAdd
                [ (Lit 5, Lit 10, Add (Lit 5) (Lit 10))
                , (Lit 0, Lit 0, Add (Lit 0) (Lit 0))
                , (Lit 0, Lit 12, Add (Lit 0) (Lit 12))
                ]
            , Test "ExprT mul test" testMul
                [ (Lit 5, Lit 10, Mul (Lit 5) (Lit 10))
                , (Add (Lit 2) (Lit 3), Lit 4, Mul (Add (Lit 2) (Lit 3)) (Lit 4))
                ]
            ]

-- Exercise 4 -----------------------------------------

ex04Tests :: [Test]
ex04Tests = [ Test "Integer lit test" testLit
                [ (5, 5 :: Integer)
                , (0, 0)
                , (-2, -2)
                ]
            , Test "Integer add test" testAdd
                [ (5, 10, 15 :: Integer)
                , (0, 0, 0)
                , (0, 12, 12)
                ]
            , Test "Integer mul test" testMul
                [ (5, 10, 50 :: Integer)
                , (6, 4, 24)
                ]
            , Test "Bool lit test" testLit
                [ (10, True)
                , (0, False)
                , (-2, False)
                ]
            , Test "Bool add test" testAdd
                [ (True, True, True)
                , (True, False, True)
                , (False, True, True)
                , (False, False, False)
                ]
            , Test "Bool mul test" testMul
                [ (True, True, True)
                , (True, False, False)
                , (False, True, False)
                , (False, False, False)
                ]
            , Test "MinMax lit test" testLit
                [ (5, MinMax 5)
                , (0, MinMax 0)
                , (-2, MinMax (-2))
                ]
            , Test "MinMax add test" testAdd
                [ (MinMax 10, MinMax 5, MinMax 10)
                , (MinMax 4, MinMax 7, MinMax 7)
                ]
            , Test "MinMax mul test" testMul
                [ (MinMax 3, MinMax 10, MinMax 3)
                , (MinMax 10, MinMax 2, MinMax 2)
                ]
            , Test "Mod7 lit test" testLit
                [ (10, Mod7 3)
                , (21, Mod7 0)
                ]
            , Test "Mod7 add test" testAdd
                [ (Mod7 5, Mod7 10, Mod7 1)
                , (Mod7 1, Mod7 2, Mod7 3)
                ]
            , Test "Mod7 mul test" testMul
                [ (Mod7 8, Mod7 2, Mod7 2)
                , (Mod7 0, Mod7 2, Mod7 0)
                ]
            ]

-- All tests ------------------------------------------

allTests :: [Test]
allTests = ex01Tests ++ ex02Tests ++ ex03Tests ++ ex04Tests
