sumOfSquares :: [Integer] -> Integer
sumOfSquares l = sum (map (^2) l)


main :: IO()
main = do
  let result =  sumOfSquares [1, 2, 3]
  print result
