-- 1. Magic numbers guys!!!

maxLetterIndex :: Int
maxLetterIndex = 26 -- You should have stored this in a constant!!!!
maxLetterIndexEvenBetter :: Int
maxLetterIndexEvenBetter = ord 'z' - ord 'a' + 1 -- Even better!!!

(<+>) :: Char -> Char -> Char
c1 <+> c2 = alphaChr ((alphaOrd c1 + alphaOrd c2) `mod` maxLetterIndex)
(<->) :: Char -> Char -> Char
c1 <-> c2 = alphaChr ((alphaOrd c1 - alphaOrd c2) `mod` maxLetterIndex)

-- 2. Encrypt was the same as decrypt
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt m (e, n) = modPow m e n

rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt = rsaEncrypt

-- 3. We prefer using @ on lists instead of head and tail
dummyBad :: [a] -> String
dummyBad xs = "The list starts with " ++ [head xs] ++ " and has " ++ show (length xs) ++ " elements."

dummyGood :: [a] -> String
dummyGood xxs@(x:_) = "The list starts with " ++ [x] ++ " and has " ++ show (length xxs) ++ " elements."

-- 4. Different methods for sorting
-- 4.1 sort
import Data.List (sort)

-- Ascending order
sort [3,1,2]
-- Result: [1,2,3]

-- Descending order
reverse (sort [3,1,2])

-- 4.2 sortOn

import Data.List (sortOn)
--sortOn :: Ord b => (a -> b) -> [a] -> [a]
-- The thing that the function passed as first argument returns should be Ord (i.e comparable)

nums :: [Int]
nums = [5,3,6,2,1,4]
sortedNums1 :: [Int]
sortedNums1 = sortOn id nums -- id is the identity function

pairs :: [(String, Int)]
pairs = [("Alice", 3), ("Bob", 1), ("Charlie", 2)]
sortedPairs1 :: [(String, Int)]
sortedPairs1 = sortOn snd pairs -- sort by the second element of the tuple

-- Off-topic: What is Ord?
-- Answer: A type class that defines a total ordering on its instances.

-- Total ordering means that for any two elements a and b, one and only one of the following is true:
-- a < b
-- a == b
-- a > b

-- What is the difference between total and partial ordering?
-- What are some examples of a partial ordering?

data Color = Red | Green | Blue deriving (Show, Eq)

data PairIntStr = PairIntStr Int String

data Down' = Down' Int
data DummyInt = DummyInt Int

instance Ord DummyInt where
    (DummyInt i1) <= (DummyInt i2) = i1 <= i2

instance Ord Down' where
    (Down' i1) <= (Down' i2) = i1 >= i2

instance Ord Color where
    Red   <= _     = True
    Green <= Red   = False
    Green <= _     = True
    Blue  <= Blue  = True
    Blue  <= _     = False

instance Ord PairIntStr where
    (PairIntStr i1 s1) <= (PairIntStr i2 s2)
        | i1 < i2   = True
        | i1 > i2   = False
        | otherwise = s1 <= s2


-- 5. $ operator for function application

-- ($) :: (a -> b) -> a -> b
-- f $ x = f x

-- Why is it useful?
-- It has low, right-associative precedence, so it can be used to avoid parentheses
-- Example:
result1 :: Int
result1 = head (tail (head (tail [[1,2,3],[4,5,6],[7,8,9]])))

result2 :: Int
result2 = head $ tail $ head $ tail [[1,2,3],[4,5,6],[7,8,9]]

-- 6. . operator for function composition
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- Example:
increment :: Int -> Int
increment x = x + 1
double :: Int -> Int
double x = x * 2

incrementThenDoubleBad :: Int -> Int
incrementThenDoubleBad x = double (increment x)

incrementThenDoubleGood :: Int -> Int
incrementThenDoubleGood = double . increment

incrementThenDoubleWithDollar :: Int -> Int
incrementThenDoubleWithDollar x = double $ increment x -- Still need to pass x. Not as good as using (.)

-- 7. Variable names

-- Never use a and b !!!!!

-- x, xs, y, ys, z, zs is good for lists and elements of lists
-- i, j, k is good for indices
-- f, g, h is good for functions but USE descriptive names when possible
-- m, n, k, l is good for integers
-- p, q, r is usually used when some division and modulo is involved