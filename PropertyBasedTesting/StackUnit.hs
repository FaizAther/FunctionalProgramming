-- UNIT TEST --------------------------S

-- Q2

-- 1)
-- i)
unitFoldl :: Bool
unitFoldl =
  stackFoldl (+) 0 Empty == 0

-- ii)
unitFoldl' :: Bool
unitFoldl' =
  stackFoldl (+) 2 (Stack (Stack (Empty :: Stack Int) 3) 4) == 9

-- 2)
-- i)
unitFoldr :: Bool
unitFoldr =
  stackFoldr (+) 5 Empty == 5

-- ii)
unitFoldr' :: Bool
unitFoldr' =
  stackFoldr (+) 4 (Stack (Stack (Empty :: Stack Int) 5) 6) == 15

-- 3)
-- i)
unitZip :: Bool
unitZip =
  stackZip (Empty :: Stack Int) (Empty :: Stack Int)
  ==
  (Empty :: Stack (Int,Int))

-- ii)
unitZip' :: Bool
unitZip' =
  stackZip (Stack (Stack (Empty :: Stack Char) 'a') 'b') (Stack (Stack (Empty :: Stack Int) 5) 6)
  ==
  Stack (Stack (Empty :: Stack (Char,Int)) ('a',5)) ('b',6)

-- 4)
-- i)
unitMap :: Bool
unitMap =
  stackMap (5 +) Empty == Empty

-- ii)
unitMap' :: Bool
unitMap' =
  stackMap (5 +) (Stack (Stack (Empty :: Stack Int) 3) 4)
  ==
  Stack (Stack (Empty :: Stack Int) 8) 9

-- UNIT TEST --------------------------E