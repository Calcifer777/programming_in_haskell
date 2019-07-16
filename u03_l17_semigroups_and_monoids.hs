import Data.List
import Data.Semigroup

-- Function composition
myLast :: [a] -> a
myLast = head . reverse

myMin :: Ord a => [a] -> a
myMin = head . sort

-- Semigroups (types with a combination element)
data Color = Blue | Brown | Green | Orange | Purple | Red | Yellow deriving (Show, Eq)

instance Semigroup Color where
    (<>) Red Blue = Purple 
    (<>) Blue Red = Purple 
    (<>) Yellow Blue = Green 
    (<>) Blue Yellow = Green 
    (<>) Red Yellow = Orange 
    (<>) Yellow Red = Orange 
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a, b] = Purple -- if both a && b are `elem` in [Red, Blue, Purple]
             | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
             | otherwise = Brown 


-- Monoids (types with an identity element)
{-
class Monoid a where 
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: a -> [a] -> [a]
-}
type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normProbs
    where probsSum = sum probs
          normProbs = map (/ probsSum) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
    show (PTable events probs) = mconcat pairs
        where pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
    where combiner = (\x y -> mconcat [x, "-", y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where 
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2)  = createPTable newEvents newProbs
        where newEvents = combineEvents e1 e2
              newProbs = combineProbs p1 p2

instance Monoid PTable where 
    mempty = PTable [] []
    mappend = (<>)

