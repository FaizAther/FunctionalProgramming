-- The University of Queensland
-- Ather, Mohammad Faiz
-- COMP3400 2021
-- Assignment 2
-- Question 2


iter :: Int -> (a -> a) -> (a -> a)
iter n f
  | n <= 0    = id
  | otherwise = iter (n-1) f . f

-- Example
--
-- f = (+) 5
-- iter 3 f == id . f . f . f
--
-- ~> (id . f . f . f) 20
-- ~> (id . f . f) 25
-- ~> (id . f) 30
-- ~> (id) 35
-- ~> 35
