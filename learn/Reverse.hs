

-- [1,2,3] ~> [3,2,1]
-- reverse [] [1,2,3]
-- reverse [1] [2,3]
-- reverse [2,1] [3]
-- reverse [3,2,1] []
-- ~> [3,2,1]

reverse' :: [Int] -> [Int] -> [Int]
reverse' a []     = a
reverse' a (x:xs) = reverse' (x:a) (xs)
