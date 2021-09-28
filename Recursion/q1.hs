-- The University of Queensland
-- Ather, Mohammad Faiz
-- COMP3400 2021
-- Assignment 2
-- Question 1

-- A)
--
expUp :: Int -> Int -> Int
expUp x y = hExpUp x y 0 1

hExpUp :: Int -> Int -> Int -> Int -> Int
hExpUp x y z ans
  | y == z    = ans
  | otherwise = hExpUp x y (z+1) (ans*x)

{-
-- B)
--
  for y >= 0 and z >= 0,

  LHS Line #1:  hExpUp x y y ans
                = ans * x ^ (y - y)
                = ans * 1
                = ans

  RHS Line #1: ans

  LHS Line #2:  hExpUp x y z ans
                = ans * x ^ (y - z)

  RHS Line #2:  hExpUp x y (z + 1) (ans * x)
                = (ans * x) * x ^ (y - (z + 1))
                = (ans * x) * x ^ (y - (z + 1))
                = (ans * x) * x ^ y * x ^ ((-z) - 1))
                = (ans * x) * x ^ y * x ^ (-z) * x ^ (-1)
                = ans * x ^ (y - z)
  
-}

{-
-- C)
--
  expUp x y = hExpUp x y 0 1 = 1 * x ^ (y - z) = x ^ y
  
-}

{-
-- D)
--
  for y >= 0 and z >= 0,

  BV(hExpUp x y z ans) = y - z

  Notice,
    BV(hExpUp x y z ans) 
      = y - z > y - (z + 1)
      = BV(hExpUp x y (z+1) (ans*x)),
  thus BV decreases at each recursive call
  and bounded below by 0 when z == y
  i.e. y - z == 0.
-}
