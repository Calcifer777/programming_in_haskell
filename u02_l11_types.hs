halve :: Integer -> Integer
halve n = n `div` 2

printDouble :: Int -> String
printDouble n = show (n*2)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) = if f x
                    then x : myFilter f xs
                    else myFilter f xs

myHead :: [a] -> [a]
myHead [] = []
myHead (x:xs) = [x]

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x