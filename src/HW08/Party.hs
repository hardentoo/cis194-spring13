{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HW08.Party where

import Data.List (intercalate)
import Data.Tree

import HW08.Employee

-- Exercise 1 -----------------------------------------

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) $ empFun e + fun

instance Monoid GuestList where
    mempty = GL [] 0
    (GL esA funA) `mappend` (GL esB funB) = GL (esA `mappend` esB) (funA + funB)

moreFun :: GuestList -> GuestList -> GuestList
moreFun glA@(GL _ funA) glB@(GL _ funB)
    | funA > funB = glA
    | otherwise   = glB

-- Exercise 2 -----------------------------------------

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node { rootLabel = root
                 , subForest = children
                 }) = f root $ treeFold f `map` children

-- Exercise 3 -----------------------------------------

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls =
    let glBossOptimum = boss `glCons` mconcat (snd `map` gls)
        glNoBossOptimum = mconcat $ uncurry max `map` gls
    in (glBossOptimum, glNoBossOptimum)

-- Exercise 4 -----------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel

-- Exercise 5 -----------------------------------------

formatGuestList :: GuestList -> String
formatGuestList (GL es fun) = "Total fun: " ++ show fun ++ "\n" ++ empNameList
  where
    empNameList = intercalate "\n" $ (show . empName) `map` es

-- Run this by invoking runhaskell inside src directory
main :: IO ()
main = readFile "HW08/company.txt" >>= putStrLn . formatGuestList . maxFun . read
