module Main where

import IC.TestSuite

import Compress
import Examples
import Data.Bag qualified as Bag
import Data.HTree

import Data.List

main :: IO ()
main = runTests tests

tests :: [TestGroup]
tests = [ testGroup "freqCount" freqCountTests
        , testGroup "merge" mergeTests
        , testGroup "Bag.insert" insertTests
        , testGroup "Bag.fromList" fromListTests
        , testGroup "occurrences" occurrencesTests
        , testGroup "reduce" reduceTests
        , testGroup "buildTree" buildTreeTests
        , testGroup "encode" encodeTests
        , testGroup "decode" decodeTests
        ]

insertTests =
  [ Bag.toList (Bag.insert 5 Bag.empty) --> [(5, 1)]
  , Bag.toList (Bag.insert 5 (Bag.insert 6 Bag.empty)) --> [(5, 1), (6, 1)]
  , Bag.toList (Bag.insert 2 (Bag.insert 2 Bag.empty)) --> [(2, 2)]
  ]

fromListTests =
  [ Bag.toList (Bag.fromList "hello world") -->
      [(' ',1),('d',1),('e',1),('h',1),('l',3),('o',2),('r',1),('w',1)]
  , Bag.toList (Bag.fromList @Bool []) --> []
  ]

freqCountTests =
  [ freqCount (Leaf 'a' 4) --> 4
  , freqCount (Node 3 (Leaf 'a' 1) (Leaf 'b' 2)) --> 3
  ]

mergeTests =
  [ merge (Leaf 'a' 3) (Leaf 'b' 1)
      --> Node 4 (Leaf 'b' 1) (Leaf 'a' 3)
  , merge (Node 5 (Leaf 'b' 2) (Leaf 'c' 3)) (Leaf 'a' 7)
      --> Node 12 (Node 5 (Leaf 'b' 2) (Leaf 'c' 3)) (Leaf 'a' 7)
  ]

occurrencesTests =
  [ sort (occurrences "mississippi") --> sort [('m',1),('i',4),('s',4),('p',2)]
  ]

reduceTests =
  [ reduce [Leaf 'm' 2,Leaf 'i' 7,Leaf 's' 7,Leaf 'p' 2,Leaf ' ' 2,Leaf 'n' 1,Leaf 'g' 1]
      --> Node 22 (Node 9 (Leaf 'm' 2) (Leaf 'i' 7)) (Node 13 (Node 4 (Leaf 'g' 1) (Node 3 (Leaf 'n' 1) (Leaf ' ' 2))) (Node 9 (Leaf 'p' 2) (Leaf 's' 7)))
  , reduce [Leaf 'n' 1,Leaf 'g' 1,Leaf 'm' 2,Leaf 'p' 2,Leaf ' ' 2,Leaf 'i' 7,Leaf 's' 7]
      --> Node 22 (Node 8 (Node 4 (Leaf 'p' 2) (Leaf ' ' 2)) (Node 4 (Node 2 (Leaf 'n' 1) (Leaf 'g' 1)) (Leaf 'm' 2))) (Node 14 (Leaf 'i' 7) (Leaf 's' 7))
  , reduce [Leaf 'a' 2, Leaf 'b' 3, Leaf 'c' 4]
      --> Node 9 (Leaf 'c' 4) (Node 5 (Leaf 'a' 2) (Leaf 'b' 3))
  ]

buildTreeTests =
  [ buildTree "aaabbbccc"
      --> Node 9 (Leaf 'c' 3) (Node 6 (Leaf 'a' 3) (Leaf 'b' 3))
  , buildTree "abcabcabcabc"
      --> Node 12 (Leaf 'c' 4) (Node 8 (Leaf 'a' 4) (Leaf 'b' 4))
  , buildTree "abcabcabc"
      --> Node 9 (Leaf 'c' 3) (Node 6 (Leaf 'a' 3) (Leaf 'b' 3))
  , buildTree "abcabcabca"
      --> Node 10 (Leaf 'a' 4) (Node 6 (Leaf 'b' 3) (Leaf 'c' 3))
  ]

encodeTests =
  [ encode fig "m" --> [0,1,1]
  , encode fig "s" --> [1,1]
  , encode fig "ss" --> [1,1,1,1]
  , encode fig "sis" --> [1,1,1,0,1,1]
  ]

decodeTests =
  [ decode fig [1,1] --> "s"
  , decode fig [1,1,1,1] --> "ss"
  , decode fig [1,1,1,0,1,1] --> "sis"
  ]
