fac_1 :: Int -> Int
fac_1 n =
  if n <= 1 then
    1
  else
    n * fac_1 (n-1)

fac_2 :: Int -> Int
fac_2 n
  | n <= 1    = 1
  | otherwise = n * fac_2 (n-1)

fac_3 :: Int -> Int
fac_3 n = aux n 1
  where
    aux n acc
      | n <= 1    = acc
      | otherwise = aux (n-1) (n*acc)

-- Pattern matching example
is_zero 0 = True
is_zero _ = False
