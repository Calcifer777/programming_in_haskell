import Data.Char
import Data.List

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

-- First past the post method
-- Conta il numero di occorrenze di x in [a]
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

-- Rimuove i duplicati da una lista
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs) 

-- Ritorna le occorrenze uniche di ogni elemento in una lista
-- e il numero di volte in cui ognune di queste appare
result :: Ord a => [a] -> [(Int, a)]
result xs = sort [(count v xs, v) | v <- rmdups(xs)]

winner :: Ord a => [a] -> a
winner = snd . last . result


-- Metodo alternativo

ballots ::   [[String]]
ballots = [["Red", "Green"],["Blue"], ["Green", "Red", "Blue"],
    ["Blue", "Green", "Red"], ["Green"]]

-- Rimuove le liste vuote da una lista di liste 
rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

-- Rimuove x da una lista
elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

-- Data una lista di preferenze, prende la prima preferenza per ogni
-- voto, conta i risultati, e ritorna la lista di opzioni
-- ordinate per il numero di voti ricevuti in modo crescente
-- più voti nella prima posizione
rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

-- We first remove empty ballots, then rank the remaining 1st-choice candidates 
-- in increasing order of votes. If only one such candidate remains, 
-- they are the winner, otherwise we eliminate the candidate 
-- with the smallest number of 1st-choice votes and repeat the process. 
winner1 :: Ord a => [[a]] -> a
winner1 bs = case rank (rmempty bs) of
    [c]-> c
    (c:cs) -> winner1 (elim c bs)

