-- 1. Basic folds

sum:: Num a => [a] -> a
sum [] = 0
sum (x:xs) = (+) x (sum xs)
sum' = foldr (+) 0

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = (*) x (product xs)
product' = foldr (*) 1

and :: [Bool] -> Bool
and [] = True
and (x:xs) = (&&) x (and xs)
and' = foldr (&&) True

or :: [Bool] -> Bool
or [] = False
or (x:xs) = (||) x (or xs)
or' = foldr (||) False

-- 2. Conclusion
-- What shape for a recursive fn do we need to turn it into foldr?

-- g = foldr f acc

-- g [] = acc
-- g (x:xs) = f x (g xs)

-- -- 3. Now let's get combine into this form
-- combine :: [Char] -> [String] -> [String]
-- combine' [] ws = ws
-- combine' (d:ds) (w:ws) = w : [d] : combine' ds ws

-- -- First let's make the arugument count correct
-- combine [] = id
-- combine' (d:ds) = \(w:ws) -> w : [d] : combine' ds ws

-- -- Now let's take out the accumulator out of the fn
-- combine' [] = id
-- combine' (d:ds) = delimit d (combine' ds)
--     where
--         delimit x next = (\(w:ws) -> w : [x] : (next ws))

combine' = foldr delimit id
    where
        delimit x next = (\(w:ws) -> w : [x] : (next ws))



-- Now let's evaluate it on something simple
-- What does combine ['+', '-', '-'] produce?
-- What happens if we apply it to ["a", "b", "c", "d"]?

-- (del '+' (del '-' (del '-' id))) ["a", "b", "c", "d"]