module Q5 where

import Prelude hiding (map, iterate)

unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
  | p x       = []
  | otherwise = h x : unfold p h t (t x)

map :: (a -> b) -> [a] -> [b]
map f = unfold null (f . head) tail

iterate :: (a -> a) -> a -> [a]
iterate f b = unfold null head (pure . f . head) [b]