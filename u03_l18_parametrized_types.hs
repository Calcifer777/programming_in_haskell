import qualified Data.Map as Map

data Organ = Heart | Kidney | Spleen | Lung | Brain deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Kidney]

organPairs :: [(Int, Organ)]
organPairs = zip [1, 7, 15, 24, 2] organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs
