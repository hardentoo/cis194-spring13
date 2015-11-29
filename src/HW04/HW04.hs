{-# OPTIONS_GHC -Wall #-}

module HW04.HW04 where

-- Exercise 1 -----------------------------------------

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate collatz
  where
    collatz n
        | even n    = n `div` 2
        | otherwise = 3 * n + 1

-- Exercise 2 -----------------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

getHeight :: Tree a -> Integer
getHeight Leaf = -1
getHeight (Node h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node _ left y right)
        | hLeft < hRight =
            let newLeft = insert x left
                hNewLeft = getHeight newLeft
                newHeight = max hNewLeft hRight + 1
            in Node newHeight newLeft y right

        | otherwise =
            let newRight = insert x right
                hNewRight = getHeight newRight
                newHeight = max hLeft hNewRight + 1
            in Node newHeight left y newRight

      where
        hLeft = getHeight left
        hRight = getHeight right

-- Exercise 3 -----------------------------------------

xor :: [Bool] -> Bool
xor = odd . foldr (\x cnt -> if x then cnt + 1 else cnt) (0 :: Integer)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

-- Exercise 4 -----------------------------------------

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1) . (*2)) $ filter (not . (`elem` crossed)) [1..n]
  where
    crossed = filter (<= n) . map transform . filter (uncurry (<=)) $ cartProd [1..n] [1..n]
    transform (i, j) = i + j + 2 * i * j
