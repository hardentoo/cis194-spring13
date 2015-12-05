{-# OPTIONS_GHC #-}

module HW08.HW08Tests where

import HW08.Employee
import HW08.Party
import Testing

-- Exercise 1 -----------------------------------------

testGlCons :: (Employee, GuestList, GuestList) -> Bool
testGlCons (e, glA, glB) = glCons e glA == glB

testMoreFun :: (GuestList, GuestList, GuestList) -> Bool
testMoreFun (glA, glB, glC) = moreFun glA glB == glC

ex01Tests :: [Test]
ex01Tests = [ Test "glCons test" testGlCons
                [ (Emp "a" 2, GL [] 0, GL [Emp "a" 2] 2)
                , (Emp "a" 4, GL [Emp "b" 3] 3, GL [Emp "a" 4, Emp "b" 3] 7)
                ]
            , Test "moreFun test" testMoreFun
                [ (GL [] 0, GL [] 0, GL [] 0)
                , (GL [Emp "a" 10] 10, GL [Emp "b" 20] 20, GL [Emp "b" 20] 20)
                ]
            ]
