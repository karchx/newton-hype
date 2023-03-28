type TriangleMaker = Char -> Int -> [String]

topLeftTriangle :: TriangleMaker
topLeftTriangle c n  = [replicate i c | i <- [n, n-1 .. 1]]

bottomLeftTriangle :: TriangleMaker
bottomLeftTriangle c n = [replicate i c | i <- [1 .. n]]

topRightTriangle :: TriangleMaker
topRightTriangle c n  = [(replicate ( n - i) ' ') ++ (replicate i c) | i <- [n, n-1 .. 1]]

bottomRightTriangle :: TriangleMaker
bottomRightTriangle c n = [(replicate ( n - i) ' ') ++ (replicate i c) | i <- [1 .. n]]

getTriangleType :: IO TriangleMaker
getTriangleType = do
  let menu = [topLeftTriangle, bottomLeftTriangle, topRightTriangle, bottomRightTriangle]
  putStr $ unlines [
      "What type of triangle do you want to print? (1,2,3 or 4)",
      "1) Top Left",
      "2) Bottom Left",
      "3) Top Right",
      "4) Bottom Right"]

  line <- getLine
  return (menu !! ((read line :: Int) - 1))

main :: IO()
main = do
    triangle <- getTriangleType
    size <- getLine
    putStr $ unlines $ triangle '*' (read size :: Int)