-- 1. Factorial
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- 2. Sum of a list
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + (sum xs)

-- 3. Length of a list
lengthList :: [a] -> Int
sumList [] = 0
lengthList (x:xs) = 1 + (sum xs)

-- 4. Sum of a tree
data Tree = Leaf Int | Node Tree Tree Int
sumTree :: Tree -> Int
sumTree (Leaf val) = val
sumTree (Node left right val) = val + sumTree right + sumTree left


-- 5. Depth of a tree
depthTree :: Tree -> Int
depthTree (Node left right _) = 1 + max (depthTree left) (depthTree right)
depthTree _ = 0