module Data.HTree where

-- Invariances:
--   * for `Node n lt rt`, `freqCount lt <= freqCount rt`
--   * the frequency in a node is the sum of *all* frequencies in the leaves below it
data HTree a = Leaf a !Int | Node !Int (HTree a) (HTree a) deriving (Show)
instance Eq (HTree a) where t1 == t2 = freqCount t1 == freqCount t2
instance Ord (HTree a) where t1 <= t2 = freqCount t1 <= freqCount t2

-- 12% of the paper
----------------------------------------------------------------------

freqCount :: HTree a -> Int
freqCount (Node n _ _) = n 
freqCount (Leaf _ n)   = n

merge :: HTree a -> HTree a -> HTree a
merge t1 t2
    | t1 > t2   =  Node freq t2 t1
    | otherwise =  Node freq t1 t2
    where freq = freqCount t1 + freqCount t2
