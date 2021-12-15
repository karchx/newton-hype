module Limits where

limit :: (Double -> Double) -> Double -> Double
limit f a = 
  head[f x | x <- [1..],
             maximum [abs (f y - f x) | y <- [x+1..x+100]] < a ]

binomialSquared ::  (Num a) => a -> a -> a -> a
binomialSquared x y = x^2 + 2*(x*y) + y^2

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
