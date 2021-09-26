module Fold where

-- Just working on some foldings

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f b []     = b
foldr' f b (x:xs) = x `f` (foldr' f b xs)

count' e =
  foldr' (\x acc -> if e==x then acc+1 else acc) 0

isAll' e = 
  foldr' (\x -> (&&) $ e==x) True

isAll'' e =
  foldr' (\x acc -> e==x && acc) True

atleastOne' e =
  foldr' (\x -> (||) $ e==x) False 



-- so basically we are chaning the base of the second call and onwards adding on
-- to the base using the base as the accumulator like 
-- foldl' (+) 0 [1,2,3]
-- foldl' (+) (0 + 1) [2,3]
-- foldl' (+) (0 + 1 + 2) [3]
-- foldl' (+) (0 + 1 + 2 + 3) []
-- now that the base case is reached the b will be retured and evaluated
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f b []     = b
foldl' f b (x:xs) = foldl' f (b `f` x) xs


-- so basically we are chaning the base of the second call and onwards adding on
-- to the base using the base as the accumulator like 
-- foldl' (+) 0 [1,2,3]
-- foldl' (+) 1 [2,3]
-- foldl' (+) 3 [3]
-- foldl' (+) 6 []
foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f b []     = b
foldl'' f b (x:xs) = let b' = b `f` x
                     in seq b' (foldl' f b' xs)
--                   in seq b' $ foldl' f b' xs
--
--                   The dollar sign $ removes
--                   the need for paranthesis.
--                   Here the b' will be reduced
--                   then foldl' will be called.
