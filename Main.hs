import Limits

main :: IO()
main = do
  let result = applyTwice (binomialSquared 2 3) 2
  print result
