
safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead []     = Nothing

data NonEmpty a = One a | Cons a (NonEmpty a)

safeHead' :: NonEmpty a -> a
safeHead' (One a) = a
safeHead' (Cons a _) = a


