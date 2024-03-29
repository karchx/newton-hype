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

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs)  = x : take' (n-1) xs

-- ANALYSIS

-- reserve' "hola"
-- reserve' "ola" ++ ['h']
-- reserve' "la" ++ ['o'] ++ ['h']
-- reserve' "a" ++ ['l'] ++ ['o'] ++ ['h']
-- reserve' "" ++ ['a'] ++ ['l'] ++ ['o'] ++ ['h']
-- reserve' => "aloh"

reserve' :: [a] -> [a]
reserve' []     = []
reserve' (x:xs) = reserve' xs ++ [x]

-- ANALYSIS 

-- zip' [1,2,3] ['a', 'b']
-- zip' [2,3] ['b'] => [(1,'a')]
-- zip' [3] [] => [(1,'a') (2,'b')]
-- zip' [] [] => [(1, 'a') (2,'b') []]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _           = []
zip' _ []           = []
zip' (x:xs) (y:ys)  = (x,y):zip' xs ys

-- ANLYSIS

-- elem' 2 [3,2,1]
-- elem' 2 == 3 | otherwise 2 [2,1]
-- elem' 2 == 2 => True

-- elem' 2 [3,4,1]
-- elem' 2 == 3 | otherwise 2 [4,1]
-- elem' 2 == 3 | otherwise 2 [1]
-- elem' 2 == 1 | otherwise 2 []
-- elem' 2 [] => False
elem' :: (Eq a) => a -> [a] -> Bool
elem' a []      = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort []    = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted  = quicksort (filter (>x) xs) 
    in smallerSorted ++ [x] ++ biggerSorted

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

oddSqureSum :: Integer
oddSqureSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]