module Blockus (tile) where

import Test.QuickCheck (Property, (==>), quickCheck)


{-

Recall we can cover the 2^k x 2^k Blockus board with V3 tiles provided one
corner is removed.

We can encode such a tiling with integers.  For example:

The 2 x 2 board:
0 1
1 1

The 4 x 4 board
0 2 3 3
2 2 1 3
4 1 1 5
4 4 5 5

Where "0" represents the removed corner.

Further notice the above boards can be represented by lists of rows:

The 2 x 2 board:
[[0,1],[1,1]]

The 4 x 4 board:
[[0,2,3,3],[2,2,1,3],[4,1,1,5],[4,4,5,5]]

Your task is to implement 
tile :: Int -> [[Int]]
which, given k returns the covering of the 2^k x 2^k board as described.

ASSUME the removed corner is ALWAYS the north west one.  That is, if your answer
is bss then bss !! 0 !! 0 == 0.

-}

q4 :: Int -> Int -> Int -> [[Int]]
q4 n m 0 = [[m]]
q4 n m 1 = [[n,n],[n,m]]
q4 n m o = joinIt a b ++ joinIt c d
  where
    o' = o - 1
    n' = n + 1
    x  = div (2^o' * 2^o' - 1) 3
    d  = q4 n'           m o'
    a  = q4 (n' + 1 * x) n o'
    b  = q3 (n' + 2 * x) n o'
    c  = q1 (n' + 3 * x) n o'

q1 :: Int -> Int -> Int -> [[Int]]
q1 n m 0 = [[m]]
q1 n m 1 = [[n,m],[n,n]]
q1 n m o = joinIt a b ++ joinIt c d
  where
    o' = o - 1
    n' = n + 1
    x  = div (2^o' * 2^o' - 1) 3
    b  = q1 n'           m o'
    a  = q4 (n' + 1 * x) n o'
    c  = q1 (n' + 2 * x) n o'
    d  = q2 (n' + 3 * x) n o'

q3 :: Int -> Int -> Int -> [[Int]]
q3 n m 0 = [[m]]
q3 n m 1 = [[n,n],[m,n]]
q3 n m o = joinIt a b ++ joinIt c d
  where
    o' = o - 1
    n' = n + 1
    x  = div (2^o' * 2^o' - 1) 3
    c  = q3 n'           m o'
    b  = q3 (n' + 1 * x) n o'
    a  = q4 (n' + 2 * x) n o'
    d  = q2 (n' + 3 * x) n o'

q2 :: Int -> Int -> Int -> [[Int]]
q2 n m 0 = [[m]]
q2 n m 1 = [[m,n],[n,n]]
q2 n m o = joinIt a b ++ joinIt c d
  where
    o' = o - 1
    n' = n + 1
    x  = div (2^o' * 2^o' - 1) 3
    a  = q2 n'           m o'
    b  = q3 (n' + 1 * x) n o'
    c  = q1 (n' + 2 * x) n o'
    d  = q2 (n' + 3 * x) n o'

joinIt :: [[Int]] -> [[Int]] -> [[Int]]
joinIt [l] [k] = [l++k]
joinIt (l:ls) (k:ks) = (l ++ k) : joinIt ls ks


tile :: Int -> [[Int]]
tile l
  | l >= 0 = q2 1 0 l
  | otherwise = []

{-
=======
EXAMPLE
=======

NOTE your solution does not have to look identical to the following examples.
We will be conducting PROPERTY TESTING of you code.  That is, we will confirm
your board satisfies the tiling rather than comparing them with tiled boards.

-- the negative number edge case
> tile (-1)
[]

> tile 1
[[0,1],[1,1]]

> tile 2
[[0,2,3,3],[2,2,1,3],[4,1,1,5],[4,4,5,5]]

-}

propLess1 :: Int -> Property
propLess1 n =
  n < 0
    ==>
  null $ tile n


prop0 :: Bool
prop0 =
  tile 0 == [[0]]

prop1 :: Bool
prop1 =
  tile 1 == [[0,1], [1,1]]

-- (head . head) (tile n) is always 0
propN :: Int -> Property
propN n =
  n >= 0 && n <= 15
    ==>
    (head . head) (tile n) == 0

propS :: Int -> Property
propS n =
  n >= 0 && n <= 10
    ==>
    length (tile n) == twoP n
    &&
    all (myF $ twoP n) (tile n)

twoP :: (Integral b, Num a) => b -> a
twoP n = 2^n

myF :: Foldable t => Int -> t a -> Bool
myF n ls = n == length ls

propMany :: Int -> Property
propMany n = (n >= 2 && n <= 10) ==> checkCenterQ2 [] 0 n $ tile n

--make check Centre for each q1, q2, q3 and q4
checkCenterQ2 :: [Int] -> Int -> Int -> [[Int]] -> Bool
checkCenterQ2 t m n ls =
  n == 1 || (checkA && checkB && checkC && checkD) -- add checkC and checkB
  where
    checkA = logicQ2 a b c d m
    checkB = checkCenterQ3 (m : t) (ht b) (n-1) b
    checkC = checkCenterQ1 (m : t) (th c) (n-1) c
    checkD = checkCenterQ2 (m : t) (hh d) (n-1) d
    a = breakA n ls
    b = breakB n ls
    c = breakC n ls
    d = breakD n ls

checkCenterQ1 :: [Int] -> Int -> Int -> [[Int]] -> Bool
checkCenterQ1 t m n ls =
  n == 1 || (checkA && checkB && checkC && checkD)
  where
    checkB = logicQ1 a b c d m
    checkA = checkCenterQ4 (m : t) (tt a) (n-1) a
    checkC = checkCenterQ1 (m : t) (th c) (n-1) c
    checkD = checkCenterQ2 (m : t) (hh d) (n-1) d
    a = breakA n ls
    b = breakB n ls
    c = breakC n ls
    d = breakD n ls

checkCenterQ3 :: [Int] -> Int -> Int -> [[Int]] -> Bool
checkCenterQ3 t m n ls =
  n == 1 || (checkA && checkB && checkC && checkD)
  where
    checkC = logicQ3 a b c d m
    checkA = checkCenterQ4 (m : t) (tt a) (n-1) a
    checkB = checkCenterQ3 (m : t) (ht b) (n-1) b
    checkD = checkCenterQ2 (m : t) (hh d) (n-1) d
    a = breakA n ls
    b = breakB n ls
    c = breakC n ls
    d = breakD n ls

checkCenterQ4 :: [Int] -> Int -> Int -> [[Int]] -> Bool
checkCenterQ4 t m n ls =
  n == 1 || (checkA && checkB && checkC && checkD)
  where
    checkD = logicQ4 a b c d m
    checkA = checkCenterQ4 (m : t) (tt a) (n-1) a
    checkB = checkCenterQ1 (m : t) (ht b) (n-1) b
    checkC = checkCenterQ3 (m : t) (th c) (n-1) c
    a = breakA n ls
    b = breakB n ls
    c = breakC n ls
    d = breakD n ls

logicQ2 :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> Bool
logicQ2 a b c d m = (hh a == m) && (ht b == th c) && (hh d ==  th c)

logicQ1 :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> Bool
logicQ1 a b c d m = (th b == m) && (tt a == th c) && (th c ==  hh d)

logicQ3 :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> Bool
logicQ3 a b c d m = (ht c == m) && (tt a == ht b) && (ht b ==  hh d)

logicQ4 :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> Bool
logicQ4 a b c d m = (tt d == m) && (tt a == ht b) && (ht b ==  th c)


breakA :: Integral b => b -> [[Int]] -> [[Int]]
breakA n ls = map (take (twoP (n-1))) (take (twoP (n-1)) ls)

breakB :: Integral b => b -> [[a]] -> [[a]]
breakB n ls = map (drop (twoP (n-1))) (take (twoP (n-1)) ls)

breakC :: Integral b => b -> [[a]] -> [[a]]
breakC n ls = map (take (twoP (n-1))) (drop (twoP (n-1)) ls)

breakD :: Integral b => b -> [[a]] -> [[a]]
breakD n ls = map (drop (twoP (n-1))) (drop (twoP (n-1)) ls)


hh :: [[Int]] -> Int
hh = head . head
th :: [[Int]] -> Int
th = last . head
ht :: [[Int]] -> Int
ht = head . last
tt :: [[Int]] -> Int
tt = last . last