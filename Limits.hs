module Limits where
import Data.Array

type Vector a = Array Int a
type Matriz a = Array (Int, Int) a

limit :: (Double -> Double) -> Double -> Double
limit f a = 
  head[f x | x <- [1..],
             maximum [abs (f y - f x) | y <- [x+1..x+100]] < a ]



binomialSquared :: Int -> Int -> IO()
binomialSquared x y = do
    let xSquaree = x^2
    let xyDouble = 2*(x*y)
    let ySquare = y^2
    print (show xSquaree ++ "x^2 + " ++ show xyDouble ++ "xy + " ++ show ySquare ++ "y^2")

listaVector :: Num a => [a] -> Vector a
listaVector xs = listArray(1,n) xs
  where n = length xs

