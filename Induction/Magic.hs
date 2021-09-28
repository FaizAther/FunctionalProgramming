module Magic (magic, sumit, prodit, catit) where

{-
Write a function with type
  magic :: (a -> a -> a) -> a -> [a] -> a
using explicit recursion (no map, fold, or zip) so that 
  sumit :: Num a => [a] -> a 
  prodit :: Num a => [a] -> a 
  catit :: [[b]] -> [b]
can be implemented as simple one-line functions invoking magic.

EXAMPLE
=======
> sumit [1,2,3,4]
10

> prodit [1,2,3,4]
24

> catit [[1,2],[3,4]]
[1,2,3,4]
-}

magic :: (a -> a -> a) -> a -> [a] -> a
magic f z []     = z
magic f z (x:xs) = let b' = f z x
                    in seq b' $ magic f b' xs

-- return the sum of the input list 
sumit :: Num a => [a] -> a
sumit = magic (+) 0

-- return the product of the input list
prodit :: Num a => [a] -> a 
prodit = magic (*) 1 

-- flatten the input list once
catit :: [[b]] -> [b]
catit = magic (++) [] 
