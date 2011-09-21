module Utils (classifyPairs, groupByKey, makeClosure, unionMap) where
import Data.List
import Data.Function
import qualified Data.Set as Set
import Data.Set (Set)

classifyPairs :: Ord b => [(a, b)] -> [(b, [a])]
classifyPairs = map remB . groupByKey snd
        where remB (b, as) = (b, map fst as)
                
groupByKey :: Ord b => (a -> b) -> [a] -> [(b, [a])]
groupByKey f = combine . sortBy (compare `on` fst) . map (\a -> (f a, [a]))
        where combine [] = []
              combine ((a,b):(c,d):xs) | a == c = combine ((a,b++d) : xs)
              combine (x:xs) = x : combine xs

makeClosure :: Eq a => (a -> a) -> a -> a
makeClosure f = fix . iterate f where
	fix (x:y:r) | x == y = x
	fix (x:r) = fix r

unionMap :: (Ord b) => (a -> Set b) -> Set a -> Set b
unionMap f = Set.fold (Set.union . f) Set.empty
