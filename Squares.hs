module Squares where

sumOfSquares :: [Integer] -> Integer
sumOfSquares l = sum (map (^2) l)

--sumOfSquares2 :: [Integer] -> Integer
--sumOfSquares2 l = [x * x | x <- l]


