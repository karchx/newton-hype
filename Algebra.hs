module Squares where

-- Sum of Squares
-- sumOfSquares 3 == 14

sumOfSquares :: Integer -> Integer
sumOfSquares n = sum [x^2 | x <- [1..n]]

-- Pythagorean Triples
-- pythagorean  :: Int -> [(Int,Int,Int)]
-- : (pythagorean n) is the list of all pythagorean triples
-- pythagorean 10  ==  [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

pythagorean :: Int -> [(Int, Int, Int)]
pythagorean n = [(x,y,z) | x <- [1..n],
                            y <- [1..n],
                            z <- [1..n],
                            x^2 + y^2  == z^2]