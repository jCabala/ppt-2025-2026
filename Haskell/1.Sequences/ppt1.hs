-- =========================================================
-- Haskell Tutorial Notes
-- =========================================================

-- Other stuff:
-- TODO: Setup GitHub repo
-- - Check GitHub Premium, Copilot & Agentic Mode setup
-- - Pick a different date for next week

-- Weekly schedule:
-- Thu 13:00–14:00  (Break & ACI Talks)
-- Thu 14:00–15:00  (Until Week 6, inclusive)
-- Thu 15:00–16:00  (Until Week 6, inclusive)
-- Fri 09:00–10:00  (As of Week 7, inclusive)
-- Fri 10:00–11:00  (As of Week 7, inclusive)
-- Fri 11:00–12:00  <- THIS ONE
-- Fri 12:00–13:00  (Horizons)
-- Fri 13:00–14:00  (Break & Horizons)
-- Fri 16:00–17:00  (Not great)

-- =========================================================
-- Coding Feedback Examples
-- =========================================================

import Data.Char (ord)

-- 1. “Nick’s favourite joke”
greaterThen :: Int -> Int -> Bool
greaterThen x y = x > y


-- 2. Magic numbers & global constants
CONSTANT_A :: Int
CONSTANT_A = 42

ALPHABET_SIZE :: Int
ALPHABET_SIZE = ord 'z' - ord 'a' + 1


-- 3. Pattern matching > guards
-- (Demonstrated below)


-- 4. Different ways to define functions

bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= 18.5 = "You are underweight."
  | bmi <= 25.0 = "You are within the normal range."
  | bmi <= 30.0 = "You are overweight."
  | otherwise   = "You are obese."
  where
    bmi = weight / height ^ 2


-- 4.1 Pattern matching in function definitions
describeList :: [a] -> String
describeList []  = "The list is empty."
describeList [_] = "The list has one element."
describeList _   = "The list has multiple elements."


-- 4.2 Pattern matching with case ... of
describeList' :: [a] -> String
describeList' xs = case xs of
  []    -> "The list is empty."
  [_]   -> "The list has one element."
  _     -> "The list has multiple elements."


-- 4.3 Guards
describeList'' :: [a] -> String
describeList'' xs
  | null xs        = "The list is empty."
  | length xs == 1 = "The list has one element."
  | otherwise      = "The list has multiple elements."


-- 4.4 if ... then ... else expressions
describeList''' :: [a] -> String
describeList''' xs =
  if null xs
    then "The list is empty."
    else if length xs == 1
      then "The list has one element."
      else "The list has multiple elements."


-- 5. where and let ... in ...
fn :: Int -> Int -> Int
fn x y = let sumXY = x + y in sumXY * 2


-- 6. Helpers in where
fnWithHelper :: Int -> Int -> Int
fnWithHelper x y = helper x + helper y
  where
    helper :: Int -> Int
    helper z = z * 2


-- 7. Comma instead of && in guards
fn' :: Int -> Int -> Int
fn' x y
  | x > 0, y > 0 = x + y
  | otherwise    = 0


-- 8. Precedence & parentheses
-- Example: (2 + 3) * 4 vs 2 + (3 * 4)


-- 9. @ operator for clarity & pattern matching
fn'' :: Show a => [a] -> String
fn'' xss@(x0 : xss2@(x1 : x2 : x3 : xs)) =
  "The first element is: " ++ show x0 ++
  " and the rest of the list is: " ++ show xss2
fn'' _ = "List too short to describe."
