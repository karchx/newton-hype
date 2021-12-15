module Squares where

sumOfSquares :: [Integer] -> Integer
sumOfSquares l = sum (map (^2) l)

secondDegreeEquations :: Double -> Double -> Double -> [Double]
secondDegreeEquations a b c = 
  [(-b+d)/n, (-b-d)/n]
  where d = sqrt(b*b-4*a*c)
        n = 2*a

absoluteValue :: (Num a, Ord a) => a -> a
absoluteValue x | x > 0 = x
                | otherwise = -x

