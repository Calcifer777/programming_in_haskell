myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (x:xs) = myDrop (n-1) xs

myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength [] = 0

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : myTake (n-1) xs

ackerman 0 n = n + 1
ackerman m 0 = ackerman (m-1) 1
ackerman m n = ackerman (m-1) (ackerman m (n-1))

collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (3*n+1)

myReverse [] = [] 
myReverse (x:xs) = myReverse xs ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

mediumFib = \n -> seq !! n 
    where seq = 0:1:[mediumFib (i-1) + mediumFib (i-2)| i <- [2..]]

    
fastFib0 _ _ 0 = 0
fastFib0 _ _ 1 = 1
fastFib0 _ _ 2 = 1
fastFib0 pp p 3 = pp + p
fastFib0 pp p c = fastFib0 p (pp + p) (c - 1)

fastFib = fastFib0 1 1