module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | n < 1 || n > 64 =  Nothing
  | otherwise = Just $ 2 ^ (n - 1) 

total :: Integer
total = maybe 0 (\n -> 2*n - 1) $ square 64
