module Utils where

capital :: String -> String
capital "" = "An empty String"
capital all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You are underweight. Are you emo?"
    | bmi <= normal = "Supposedly you're normal...I hope you're ugly"
    | bmi <= fat    = "You are fat! lose some chubby weight"
    | otherwise     = "Congratulation, you are a whale!"
    where bmi = weight / height ^ 2
          skinny = 18.2
          normal = 25.0
          fat = 30.0


max' :: (Ord a) => a -> a -> a
max' a b
 | a < b      = b
 | otherwise  = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b  = GT
    | a == b = EQ
    | otherwise = LT


initials :: String -> String -> String
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

head' :: [a] -> a
head' xs = case xs of [] -> error "head doesn't work empty lists"
                      (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is" ++ what xs
    where what []  = " empty."
          what [x] = " a singleton list."
          what xs  = " a lorge list"