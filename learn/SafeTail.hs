
safeTail :: [a] -> Maybe a
safeTail []     = Nothing
safeTail [x]    = Just x
safeTail (x:xs) = safeTail xs
