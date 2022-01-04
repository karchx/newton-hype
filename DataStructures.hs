module DataStructures where

-- Busqueda secuencial
busSec :: Ord a => [a] -> a -> Bool
busSec [] _ = False
busSec (x:xs) ele
  | x == ele = True
  | True = busSec xs ele

-- Busqueda binaria
busBin :: Ord a => [a] -> a -> Int -> Int -> Bool
busBin [] _ _ _ = error("Please check your code")
busBin vector ele ini fin
  | ini > fin = False
  | ele == vector!!medio = True
  | ele > vector!!medio = busBin vector ele (medio + 1) fin
  | True = busBin vector ele ini (medio - 1)
  where
    medio = (div(ini+fin)2)
