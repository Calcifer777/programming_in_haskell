myMap _ [] = []
myMap f (x:xs) = f x : map f xs
-- myMap (+1) [1 .. 10]

myFilter _ [] = []
myFilter f (x:xs) = if f x
                    then x : myFilter f xs
                    else myFilter f xs
-- myFilter  (>5) [1 .. 10]
-- myFilter  (\(x:xs) -> x == 'a') ["apple" "banana" "avocado"]

myRemove _ [] = []
myRemove test (x:xs) = if test x
                       then myRemove test xs
                       else x : myRemove test xs


myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
    where newInit = f init x

myFoldr f init [] = init
myFoldr f init (x:xs) = f x newList
    where newList = myFoldr f init xs

myElem x aList = length (filter (== x) aList) > 0

-- q9.2
isPalindrome sentence = newSentence == reverse newSentence
    where newSentence = filter (/= ' ') sentence

-- q9.3
harmonic n = sum(take n series)
    where pairs = zip (repeat 1.0) [1.0, 2.0 ..]
          series = map (\pair -> fst pair / snd pair) pairs