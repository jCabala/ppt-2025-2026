module Data.Bag (Bag, insert, empty, toList, fromList) where

-- Data.Map.Strict has the same API as Data.Map, but the values stored are
-- strictly stored (which is good for Ints!)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- Invariant: Ints in the Map are >= 1
newtype Bag a = Bag (Map a Int)

instance Show a => Show (Bag a) where show (Bag m) = show m

empty :: Bag a -- O(1)
empty = Bag Map.empty

toList :: Bag a -> [(a, Int)] -- O(n)
toList (Bag m) = Map.toList m

-- 16% of the paper
----------------------------------------------------------------------

insert :: Ord a => a -> Bag a -> Bag a -- O(log n)
insert c (Bag m) = Bag (Map.insertWith (+) c 1 m)

fromList :: Ord a => [a] -> Bag a -- O(n log n)
fromList = foldr insert empty
