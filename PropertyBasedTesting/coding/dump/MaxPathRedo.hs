module MaxPathRedo where

import Test.QuickCheck (Property, (==>), quickCheck)

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
            deriving Show

data TreePath = End Int
              | Many ([Int], [Int], [Int], [Int])
              deriving Show

getPath :: TreePath -> Char -> [Int]
getPath (Many (f,l,r,c)) 'F' = f
getPath (Many (f,l,r,c)) 'L' = l
getPath (Many (f,l,r,c)) 'R' = r
getPath (Many (f,l,r,c)) 'C' = c
getPath (Many (f,l,r,c))  _  = []

calcPath :: Tree Int -> TreePath
calcPath (Leaf n)     = End n
calcPath (Node l n r) =
  tp
  where
    l'  = calcPath l
    r'  = calcPath r
    tp  = addAll' n l' r'

maximum'' :: Ord a => [a] -> [a]
maximum'' [] = []
maximum'' xs = [maximum xs]

myFoldL :: Int -> [Int] -> [Int] -> [Int]
myFoldL n [] acc = acc
myFoldL n (l:ls) [] = myFoldL n ls [n + l]
myFoldL n (l:ls) acc
  | n + l > head acc = myFoldL n ls [n + l]
  | otherwise  = myFoldL n ls acc

addAll' :: Int -> TreePath -> TreePath -> TreePath
addAll' n (End l) (End r) =
  Many ([n], [n+l], [n+r], [n+l+r])
addAll' n (End l) rs      =
  addAll' n ls rs
  where
    ls = Many ([l], [], [], [])
addAll' n ls      (End r) =
  addAll' n ls rs
  where
    rs = Many ([r], [], [], [])
addAll' n ls      rs      =
  Many (ns', ls'', rs'', cs')
  where
    ns'  =  maximum'' $ n : nsLR
    nsLR =  nsL ++ nsR
    nsL  =  myFoldL n (getPath ls 'F') []
    nsR  =  myFoldL n (getPath rs 'F') []
    ls'' =  maximum'' $ getPath ls 'L' ++ getPath ls 'R' ++ ls'
    ls'  =  maximum'' $ myFoldL n (getPath ls 'L') [] ++ myFoldL n (getPath ls 'R') []
    rs'' =  maximum'' $ getPath rs 'L' ++ getPath rs 'R' ++ rs'
    rs'  =  maximum'' $ myFoldL n (getPath rs 'L') [] ++ myFoldL n (getPath rs 'R') []
    cs   =  maximum'' $ getPath ls 'C' ++ getPath rs 'C'
    cs'  =  maximum'' $ cs ++ crossApply n (maximum'' (nsL ++ ls')) (maximum'' rs') []
            where
              crossApply :: Int -> [Int] -> [Int] -> [Int] -> [Int]
              crossApply n []      _ cs = cs
              crossApply n _      [] cs = cs
              crossApply n (a:as) bs cs =
                cx
                where
                  cf = myFoldL (a - n) bs []
                  cx = crossApply n as bs (checkCs cf cs)
              -- check the cs
              checkCs :: [Int] -> [Int] -> [Int]
              checkCs [] [] = []
              checkCs [] cx = cx
              checkCs cf [] = cf
              checkCs [cf] [cx]
                | cf >= cx  = [cf]
                | otherwise = [cx]

maxPath :: Tree Int -> Int
maxPath t =
  maximum' (calcPath t)
  where
    maximum' (End n)              = n
    maximum' (Many (fs,ls,rs,cs)) = maximum (fs ++ ls ++ rs ++ cs)

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

regTest :: Bool
regTest =
  maxPath treeA == 6  &&
  maxPath treeB == 42 &&
  maxPath treeC == 24 &&
  maxPath (treeGenLeft 20 1) == 20 &&
  maxPath (treeGenRight 20 1) == 20 &&
  maxPath (treeGenLR 10 1) == 19 &&
  maxPath (treeGenZero 2 0 1) == 3

-- property idea
-- if its a mirror tree then
--
--
qcAll :: Int -> Int -> Property
qcAll n m =
  n >= 0 && n < 20 ==>
    (maxPath (treeGenLeft n m) + maxPath (treeGenRight n m) - m)
    ==
    maxPath (treeGenLR n m)


sumAll :: Tree Int -> Int
sumAll (Leaf a) = a
sumAll (Node l a r) = sumAll l + a + sumAll r


testSum :: Int -> Int
testSum n = sumAll $ treeGenLR n 1