module CartProd (cartProd) where

{-
*DO NOT* use
  map, zip, foldl, or foldr
to solve this question 

*DO NOT* load any modules.

Given a list of lists [xs0, xs2, ..., xsk], return the CARTESIAN PRODUCT of 
those lists.

=======
EXAMPLE
=======
> cartProd [[1, 2], [3], [4, 5]]
[[1, 3, 4], [1, 3, 5], [2, 3, 4], [2, 3, 5]]
-}

magic :: ([[a]] -> [a] -> [[a]]) -> [[a]] -> [[a]] -> [[a]]
magic _ b []      = b
magic f b ([]:ls) = []
magic f b (l:ls)  = let b' = f b l
                     in seq b' $ magic f b' ls

cartProd :: [[a]] -> [[a]]
cartProd []   = []
cartProd l    = magic myf [[]] l

myf :: [[a]] -> [a] -> [[a]]
myf l      []     = l
myf [[]]   [m]    = [[m]]
myf [[]]   (m:ms) = myf [[]] [m] ++ myf [[]] ms
myf [l]    [m]    = [l++[m]]
myf (l:ls) [m]    = (l ++ [m]) : myf ls [m]
myf [l]    (m:ms) = (l++[m]) : myf [l] ms
myf (l:ls) (m:ms) = myf [l] (m:ms) ++ myf ls (m:ms)
