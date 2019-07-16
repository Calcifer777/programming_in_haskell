myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
    where remainder = mod a b

myHead (x:xs) = x

myTail (_:xs) = xs
myTail [] = []

myGCDpm _ 0 = 0
myGCDpm 0 _ = 0
myGCDpm a b = if remainder == 0
              then b
              else myGCD b remainder
    where remainder = mod a b
