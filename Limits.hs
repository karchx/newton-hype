module Limits where

limit :: (Double -> Double) -> Double -> Double
limit f a = 
  head[f x | x <- [1..],
             maximum [abs (f y - f x) | y <- [x+1..x+100]] < a ]

