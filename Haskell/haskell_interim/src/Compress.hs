module Compress where

import Data.List
import Data.Char

import Data.Bag (Bag)
import Data.Bag qualified as Bag
import Data.HTree

import Examples -- for cabal repl

import Data.Maybe (listToMaybe) -- Added this import for listToMaybe

type Code = [Int]

-- 32% of the paper
----------------------------------------------------------------------

occurrences :: Ord a => [a] -> [(a, Int)]
occurrences = Bag.toList . Bag.fromList

reduce :: [HTree a] -> HTree a
-- Pre: The argument list is non-empty and sorted based on `Ord HTree`
reduce = foldr1 merge

buildTree :: Ord a => [a] -> HTree a
-- Pre: The list is non-empty
buildTree = reduce . map (uncurry Leaf) . occurrences

-- 28% of the paper
----------------------------------------------------------------------

encode :: Eq a => HTree a -> [a] -> Code
-- Pre: The tree can encode each of the items the list
encode tree = concatMap getCode
    where
        pseudoMap = buildMap tree -- Stores the code for each character. Just a list of pairs.

        buildMap :: Eq a => HTree a -> [(a, Code)]
        buildMap (Leaf x _) = [(x, [])]
        buildMap (Node _ l r) = map (addBit 0) (buildMap l) ++ map (addBit 1) (buildMap r)
            where addBit b (x, code) = (x, b : code)

        lookup :: Eq a => a -> [(a, Code)] -> Code
        lookup a mapping = head [code | (x, code) <- mapping, x == a]
        
        getCode x = lookup x pseudoMap


decode :: HTree a -> Code -> [a]
-- Pre: The code is valid with respect to the tree
decode tree = go tree
    where
        go (Node _ l _) (0:bits) = go l bits
        go (Node _ _ r) (1:bits) = go r bits
        go (Leaf x _) bits       = x : go tree bits
        go _ []                  = []


-- The fold version still doesn't seem to work correctly. If someone sees the bug, please fix it.
-- decode tree = fst . (foldr step ([], tree)) -- pair of result and current node
--     where
--         step bit (result, currentNode) = case nextNode of
--             Leaf x _ -> (x : result, tree) -- reset to root
--             _        -> (result, nextNode)
--             where
--                 nextNode = stepTree bit currentNode    

--         stepTree bit (Node _ l r) = if bit == 0 then l else r

-- 12% of the paper
----------------------------------------------------------------------

compressTree :: HTree Char -> [Int]
compressTree = undefined

rebuildTree :: [Int] -> HTree Char
-- Pre: The bitstring ([Int]) is a valid encoding of a Huffman tree
--      of characters
rebuildTree = undefined
