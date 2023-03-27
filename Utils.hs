module Utils where

capital :: String -> String
capital "" = "An empty String"
capital all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "You are underweight. Are you emo?"
    | weight / height ^ 2 <= 25.0 = "Supposedly you're normal...I hope you're ugly"
    | weight / height ^ 2 <= 30.0 = "You are fat! lose some chubby weight"
    | otherwise                   = "Congratulation, you are a whale!"

    