-- The University of Queensland
-- Ather, Mohammad Faiz
-- COMP3400 2021
-- Assignment 2
-- Question 3


foo :: a -> [a] -> a
foo = foldl (curry snd)

-- A)
--
-- Returns the last element of a list for a non empty list
-- for an empty list returns the first argument provided.

-- B)
--
foo' :: a -> [a] -> a
foo' = foldl (\_ x -> x)

foo'' :: a -> [a] -> a
foo'' x [] = x
foo'' _ xs = last xs

prop :: Eq a => a -> [a] -> Bool
prop b xs = foo' b xs == foo'' b xs 
