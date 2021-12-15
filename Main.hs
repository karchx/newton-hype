import Limits

secondDegreeEquations :: Double -> Double -> Double -> [Double]
secondDegreeEquations a b c = 
  [(-b+d)/n, (-b-d)/n]
  where d = sqrt(b*b-4*a*c)
        n = 2*a

absoluteValue :: (Num a, Ord a) => a -> a
absoluteValue x | x > 0 = x
                | otherwise = -x

test :: Int -> Int
test n | n > 0 = (n-1)

main :: IO()
main = do
  -- let result = limit (\n -> (2*n+1)/(n+5)) 0.001 
  let result2 = binomialSquared (-2) (-3)
  -- let result =  sumOfSquares [1, 2, 3]
  -- let resultTest = secondDegreeEquations 1 3 2
  -- let resultAbsolute = absoluteValue (-4567899)
  -- let result2 = test 100
  print result2
