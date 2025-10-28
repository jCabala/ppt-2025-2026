iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

expand :: Rules Char -> [Char] -> Int -> [Char]
-- expand rules cs n applies (expandOne rules) n times to cs
-- expand rules cs 0 = cs
-- expand rules cs n = expand rules (expandOne rules cs) (n - 1)

expand rules cs n = iterate (expandOne rules) cs !! n
