-- 1. Tail recursion for computing Fibonacci numbers
betterFib :: Int -> Integer
betterFib n = go n 0 1
  where
    go 0 a _ = a
    go k a b = go (k - 1) b (a + b)
-- Btw, go is a approved name for helper functions like this (i.e tail recursive helper)

-- 2. Using infinite lists and lazy evaluation to compute Fibonacci numbers efficiently
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib1 :: Int -> Integer
fib1 n = fibs !! n