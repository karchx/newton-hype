import Limits

main :: IO()
main = do
  let result = limit (\n -> (2*n+1)/(n+5)) 0.001 
  print result
