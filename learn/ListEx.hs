-- List Exercises from Haskell for Imperative Programmers


--  Takes in a element a and chekes
--  if it is in the given list
--
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []     = False
elem' e (x:xs) = e == x || elem' e xs


-- Remove duplicates from a given list
--
nub' :: (Eq a) => [a] -> [a]
nub' []         = []
nub' (x:xs)
  | x `elem` xs = nub' xs
  | otherwise   = x : nub' xs

-- Return True if ascending list given
--
isAsc :: [Int] -> Bool
isAsc []          = True
isAsc [x]         = True
isAsc (x:(y:xs))  = x < y && isAsc (y:xs)
--isAsc (x:y:xs)  = x < y && isAsc (y:xs)

-- Graph exercise using connections as list tuples
-- Does a path exist
-- [(1,2),(2,3),(3,2),(4,3),(4,5)] -> 1 -> 3 -> True
--
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath []  _ _     = False
hasPath [x] a b     = (fst x) == a && (snd x) == b
hasPath (x:xs) a b
  | (fst x) == a && (snd x) == b = True
  | fst x == a = hasPath xs (snd x) b
  | otherwise  = hasPath xs a b



