-- q6.1
subseq start stop aList = drop start (take stop aList)

-- q6.2
repeat n = cycle [n]

-- q6.3
inFirstHalf x aList = x `elem` take (length aList `div` 2) aList