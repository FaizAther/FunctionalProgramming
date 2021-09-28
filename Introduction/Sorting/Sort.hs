--  Sorting

--  T(n) = O(n^2) time complexity
insert :: Int -> [Int] -> [Int]
insert x []     = [x]
insert x (y:ys)
  | (x <= y)  = x : y : ys
  | otherwise = y : insert x ys

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

isort' :: [Int] -> [Int]
isort' xs = foldr insert [] xs

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | (x <= y)  = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

msort :: [Int] -> [Int]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort (front xs))
                  (msort (back  xs))
  where
    front xs = take (length xs `div` 2) xs
    back  xs = drop (length xs `div` 2) xs


qsort :: [Int] -> [Int]
qsort []     = []
qsort (x:xs) = (qsort lower) ++
               [pivot] ++
               (qsort upper)
  where
    pivot = x
    lower = [y | y <- xs, y <= x]
    upper = [y | y <- xs, y >  x]

