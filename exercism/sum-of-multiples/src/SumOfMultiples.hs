module SumOfMultiples (sumOfMultiples) where

import Data.List ( foldl' )

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = foldl' someF 0 ls
  where
    ls = [1..limit-1]
    someF i j = i + anyFac j
    anyFac n
      | any (isFac n) factors = n
      | otherwise             = 0
    isFac p q = q /= 0 && rem p q == 0
