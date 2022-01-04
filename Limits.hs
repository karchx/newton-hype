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


listaMatriz :: Num a => [[a]] -> Matriz a
listaMatriz xss = listArray ((1,1), (m,n)) (concat xss)
  where m = length xss
        n = length (head xss)

numFilas :: Num a => Matriz a -> Int
numFilas = fst . snd . bounds

numColumnas :: Num a => Matriz a -> Int
numColumnas = snd . snd . bounds

dimension :: Num a => Matriz a -> (Int, Int)
dimension = snd . bounds

-- ----------------------------------------------------------------------
-- Suma de matrices
-- ----------------------------------------------------------------------
sumaMatrices :: Num a => Matriz a -> Matriz a -> Matriz a
sumaMatrices p q =
  array ((1,1), (m,n)) [((i,j),p!(i,j)+q!(i,j))
                       | i <- [1..m], j <- [1..n]]
  where (m,n) = dimension p


identidad :: Num a => Int -> Matriz a
identidad n =
  array ((1,1), (n,n))
        [((i,j), f i j) | i <- [1..n], j <- [1..n]]
  where f i j | i == j = 1
              | otherwise = 0
