sumOfSquares :: [Integer] -> Integer
sumOfSquares l = sum (map (^2) l)

sumOfSquares2 :: [Integer] -> Integer
sumOfSquares2 l = [x * x | x <- l]


main :: IO()
main = do
  let result =  sumOfSquares [1, 2, 3]
  let result2 =  sumOfSquares2 [1, 2, 3]
  print result2
