-- 1. splitText could have been a fold
splitText :: [Char] -> String -> ([Char], [String])
splitText delims = foldr splitOn ([], [""])
    where
        splitOn c (ds, words@(w : ws))
            | elem c delims = (c : ds, "" : words)
            | otherwise = (ds, (c : w) : ws)

-- 2. Combine using higher order functions (intuitive version)
combine1 :: [Char] -> [String] -> [String]
combine1 ds (w : ws) = w : concat (zipWith (\d w -> [[d], w]) ds ws) -- normal zip wouldn't work because of [d] and [[d], w] instead of d and [d, w]

-- 2.1 zip and map version of combine
combine2 :: [Char] -> [String] -> [String]
combine2 ds wws@(w : ws) = w : concatMap (\(d, w) -> [[d], w]) (zip ds ws)

-- 3. combine could have been a single fold (however I don't see why you would ever want to do this)
-- This is called "foldr with function composition"
-- The foldr is used to build a big function that is then applied to the list

combine3 :: [Char] -> [String] -> [String]
combine3 = foldr delimit id
    where
        delimit :: Char -> ([String] -> [String]) -> [String] -> [String] 
        delimit d next (w : ws) = w : [d] : next ws

-- (delimit '-' (delimit '+' id)) ["a","b","c"]

-- 4. Errors in Haskell
errorOnEmpty :: [a] -> a
errorOnEmpty []    = error "Empty list!"
errorOnEmpty (x:_) = x