
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' f (x:xs) = if f x == True
                   then x : filter f xs
                   else filter f xs
