
in_range_1 min max x = x >= min && x <= max 

in_range_2 min max x = (&&) ((>=) x min) ((<=) x max)

in_range_3 min max x = l_b && u_b
  where
  l_b = x >= min
  u_b = x <= max

in_range_4 min max x =
  let l_b = x >= min
      u_b = x <= max
  in l_b && u_b

add_1 x y = x + y

add_2 = \x y -> x + y

lower_b :: Int -> Int -> Bool
lower_b = \min x -> x >= min

upper_b :: Int -> Int -> Bool
upper_b = \max x -> x <= max

in_range_5 :: Int -> Int -> Int -> Bool
in_range_5 = \min max x -> ((&&) (lower_b min x) (upper_b max x))
