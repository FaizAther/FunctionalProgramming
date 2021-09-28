module MaxPath where

import Test.QuickCheck (Property, (==>), quickCheck)

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
            deriving Show

maxP :: Tree Int -> (Int, Int)
maxP (Leaf a) = (a, a)
maxP (Node l a r) = (xs,ys)
                    where
                      (l',lc) = maxP l
                      (r',rc) = maxP r
                      lr  = maximum [l',r']
                      lr' = lr + a
                      alr = l' + r' + a
                      ys  = maximum [lr,alr,lc,rc]
                      xs  = maximum [a,lr']

maxPath :: Tree Int -> Int
maxPath t = maximum [xs,ys]
            where
              (xs,ys) = maxP t

-- Test

treeA :: Tree Int
treeA = Node (Leaf 1) 2 (Leaf 3)

treeB :: Tree Int
treeB = Node (Leaf 9) (-10) (Node (Leaf 15) 20 (Leaf 7))

treeBL :: Tree Int
treeBL = Node (Leaf 15) 20 (Leaf 7)

treeCL :: Tree Int
treeCL = Node (Leaf 1) 5 (Leaf 2)

treeCR :: Tree Int
treeCR = Node (Leaf 3) 6 (Leaf 4)

treeC :: Tree Int
treeC = Node treeCL 7 treeCR

regTest :: Bool
regTest =
  maxPath treeA == 6  &&
  maxPath treeB == 42 &&
  maxPath treeC == 24 &&
  maxPath (treeGenLeft 20 1) == 20 &&
  maxPath (treeGenRight 20 1) == 20 &&
  maxPath (treeGenLR 10 1) == 19 &&
  maxPath (treeGenZero 2 0 1) == 3

treeZero :: Tree Int
treeZero = Node (Leaf 0) 0 (Leaf 0)

treeGenZero :: Int -> Int -> Int -> Tree Int
treeGenZero 0 m c = Node (Leaf m) m (Leaf m)
treeGenZero n m c = Node l m r
                    where
                      l = treeGenZero (n - 1) m (c - 1)
                      r = if n > 1 && c == 1
                          then treeGenZero (n - 1) m c
                          else treeGenZero (n - 1) c c

treeGenRight :: Int -> Int -> Tree Int
treeGenRight n m
  | n <= 1    = Leaf m
  | otherwise = Node (Leaf 0) m (treeGenRight (n - 1) m)

treeGenLeft :: Int -> Int -> Tree Int
treeGenLeft n m
  | n <= 1    = Leaf m
  | otherwise = Node (treeGenLeft (n - 1) m) m (Leaf 0)

treeGenLR :: Int -> Int -> Tree Int
treeGenLR n m
  | n <= 1    = Leaf m
  | otherwise = Node t m t
                where
                  t = treeGenLR (n - 1) m

qcBoundary :: Int -> Int -> Property
qcBoundary n m =
  m >= 0 && n >= 0 && n < 20 ==>
    (maxPath (treeGenLeft n m) + maxPath (treeGenRight n m) - m)
    ==
    maxPath (treeGenLR n m)

treeGen :: [Int] -> Tree Int
treeGen [m] = Leaf m
treeGen (m:ms) = Node t m t
                where
                  t = treeGen ms

mirrorTree :: Tree Int -> Tree Int
mirrorTree (Leaf a) = Leaf a
mirrorTree (Node l a r) = Node (mirrorTree r) a (mirrorTree l)

qcMirror :: [Int] -> Property
qcMirror xs =
  not (null xs) && length xs <= 15 ==>
  ml == ml'
  where
    l = treeGen xs
    ml = maxPath l
    l' = mirrorTree l
    ml' = maxPath l'


{-
-- finds triangle
> o = Node (Node (Node (Leaf 0) 0 (Leaf 0)) 0 (Node (Leaf 0) 0 (Leaf 0))) 0 (Node (Node (Node (Leaf 0) 0 (Leaf 0)) 0 (Node (Leaf 0) 0 (Leaf 0))) 0 (Node (Node (Node (Leaf 0) 0 (Leaf 0)) 1 (Node (Leaf 0) 0 (Leaf 0))) 1 (Node (Node (Leaf 0) 0 (Leaf 0)) 1 (Node (Leaf 0) 0 (Leaf 0)))))
> 3 == maxPath o
True

-- finds zero in a sea of negatives
> z = Node (Node (Node (Leaf (-1)) (-1) (Leaf (-1))) (-1) (Node (Leaf (-1)) (-1) (Leaf (-1)))) (-1) (Node (Node (Node (Leaf (-1)) (-1) (Leaf (-1))) (-1) (Node (Leaf (-1)) (-1) (Leaf (-1)))) (-1) (Node (Node (Node (Leaf (-1)) (-1) (Leaf (-1))) (-1) (Node (Leaf (-1)) (-1) (Leaf (-1)))) 0 (Node (Node (Leaf (-1)) (-1) (Leaf (-1))) (-1) (Node (Leaf (-1)) (-1) (Leaf (-1))))))
> 0 == maxPath z
True
-}


-- LR Node (Leaf (-1)) (-1) (Leaf (-1))
-- L  Node (Leaf (-1)) (-1) (Leaf 0)
-- R  Node (Leaf 0) (-1) (Leaf (-1))

testSum :: Int -> Int
testSum n = sumAll $ treeGenLR n 1

sumAll :: Tree Int -> Int
sumAll (Leaf a) = a
sumAll (Node l a r) = sumAll l + a + sumAll r