module Algebra where

-- Sum of Squares
-- sumOfSquares 3 == 14

sumOfSquares :: Integer -> Integer
sumOfSquares n = sum [x ^ 2 | x <- [1 .. n]]

-- Pythagorean Triples
-- pythagorean  :: Int -> [(Int,Int,Int)]
-- : (pythagorean n) is the list of all pythagorean triples
-- pythagorean 10  ==  [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n =
  [ (x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2
  ]

-- Factorial
-- factorial 5 = 5 * 4 * 3 * 2 * 1 == 120
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * (x -1)

-- second degree equations
-- x = -b +- sqrt(b^2 - 4*a*c) / 2*a-

square :: Float -> Float
square n = n * n

equationSecond :: Float -> Float -> Float -> Float
equationSecond a b c = (- b + sqrt (square b - (4 * a * c))) / (2 * a)

-- Difference of squares
-- a^2 - b^2 = (a+b)(a-b)
-- differenceSquares :: Int -> Int -> Int
-- differenceSquares a b = (a_b)
