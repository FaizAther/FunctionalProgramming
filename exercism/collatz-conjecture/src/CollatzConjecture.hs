module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | n == 1 = Just 0
  | otherwise = Just $ collatz' n 0
     where collatz' n' a
             | n' == 1        = a
             | even n'  = collatz' (n' `div` 2)       a+1
             | otherwise      = collatz' ((3*n'+1) `div` 2) a+2
