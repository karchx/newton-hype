module Kepler where

semidistance :: Integer -> Integer -> Float
semidistance a b = do
  let a' = a ^ 2
  let b' = b ^ 2
  sqrt (fromInteger (a' - b'))

eccentricity :: Integer -> Float
eccentricity a = semidistance a 3 / fromInteger a
