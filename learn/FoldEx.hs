import Fold

--  Reverse a given list using fold
rev :: [a] -> [a]
rev = foldl'' (\acc x -> (:) x acc) []


--  Prefixes
--  prefixes [1,2,3]
--  ~> [[1],[1,2],[1,2,3]]
prefixes' :: [a] -> [[a]]
prefixes' = foldr' (\x acc -> [x] : (map ((:) x) acc)) [] 
