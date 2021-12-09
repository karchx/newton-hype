import Data.Maybe
import Data.List
import Data.Functor
import Control.Arrow

sumOfSquares :: [Integer] -> Integer
sumOfSquares l = sum (map (^2) l)


main :: IO()
main = do
  result <- sumOfSquares [1, 2, 3]
  putStrLn result
