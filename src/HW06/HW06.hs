{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module HW06.HW06 where

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fib `map` [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = a `Cons` (Stream a)

streamToList :: Stream a -> [a]
streamToList (a `Cons` as) = a : streamToList as

instance Show a => Show (Stream a) where
    show as = let asList = take 20 . streamToList $ as
              in "Stream " ++ show asList

-- Exercise 4 -----------------------------------------

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (a `Cons` as) = f a `Cons` streamMap f as

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed g a = a `Cons` streamFromSeed g (g a)

-- Exercise 5 -----------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (a `Cons` as) bs = a `Cons` interleaveStreams bs as

ruler :: Stream Integer
ruler = interleaveStreams zeros $ streamMap (+1) ruler
  where
    zeros = streamRepeat 0

-- Exercise 6 -----------------------------------------

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = Cons n $ streamRepeat 0
    negate (a `Cons` as) = negate a `Cons` negate as
    (a `Cons` as) + (b `Cons` bs) = (a + b) `Cons` (as + bs)
    (a `Cons` as) * allB@(b `Cons` bs) = (a * b) `Cons` (streamMap (a*) bs + as * allB)

instance Fractional (Stream Integer) where
    (a `Cons` as) / (b `Cons` bs) = q
      where
        q = (a `div` b) `Cons` streamMap (`div` b) (as - q * bs)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

-- Exercise 7 -----------------------------------------

newtype Matrix = Matrix (Integer, Integer, Integer, Integer)

instance Num Matrix where
    (Matrix (x1, x2, x3, x4)) * (Matrix (y1, y2, y3, y4)) =
        Matrix ( x1*y1 + x2*y3
               , x1*y2 + x2*y4
               , x3*y1 + x4*y3
               , x3*y2 + x4*y4
               )

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = let m = Matrix (1, 1, 1, 0)
             (Matrix (_, ans, _, _)) = m ^ n
         in ans
