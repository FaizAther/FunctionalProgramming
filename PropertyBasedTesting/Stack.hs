{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Stack (stackFoldl, stackFoldr, stackZip, stackMap) where

import Test.QuickCheck (Property, (==>), quickCheck, Arbitrary (arbitrary))


data Stack a  = Empty
              | Stack (Stack a) a
              deriving (Show, Eq)

stackFoldl :: (b -> a -> b) -> b -> Stack a -> b
stackFoldl f b Empty       = b
stackFoldl f b (Stack s a) = stackFoldl f (b `f` a) s

stackFoldr :: (a -> b -> b) -> b -> Stack a -> b
stackFoldr f b Empty       = b
stackFoldr f b (Stack s a) = a `f` stackFoldr f b s

stackZip :: Stack a -> Stack b -> Stack (a,b)
stackZip Empty        _            = Empty
stackZip _            Empty        = Empty
stackZip (Stack sa a) (Stack sb b) = Stack (stackZip sa sb) (a,b)

stackMap :: (a -> b) -> Stack a -> Stack b
stackMap f Empty       = Empty
stackMap f (Stack s a) = Stack (stackMap f s) $ f a

-- PROPERTY TEST --------------------------------------------S

genStack :: [a] -> Stack a
genStack []     = Empty
genStack (x:xs) = Stack (genStack xs) x

stackToL :: Stack a -> [a]
stackToL Empty       = []
stackToL (Stack s a) = a : stackToL s

-- test with + - * div
qcFoldl :: Eq b => (b -> a -> b) -> b -> [a] -> Bool
qcFoldl f z ls =
  l == c
  where
    s = genStack ls
    c = foldl f z ls
    l = stackFoldl f z s

qcFoldlDiv :: Int -> [Int] -> Property
qcFoldlDiv z ls =
  z /= 0 && all (\a -> a /= 0) ls ==> qcFoldl (div) z ls

{-

*Stack> quickCheck $ qcFoldl (+)
+++ OK, passed 100 tests.
*Stack> quickCheck $ qcFoldl (-)
+++ OK, passed 100 tests.
*Stack> quickCheck $ qcFoldl (*)
+++ OK, passed 100 tests.
*Stack> quickCheck $ qcFoldlDiv 
+++ OK, passed 100 tests; 40 discarded.

-}

-- Requires Commutative Opeartions +, *
qcFoldl1 :: Eq a => (a -> a -> a) -> a -> [a] -> Bool
qcFoldl1 f z ls =
  l' == r' && c == l'
  where
    s = genStack ls
    l' = stackFoldl f z s
    r' = stackFoldr f z s
    c = foldl f z ls

{-

*Stack> quickCheck $ qcFoldl1 (*)
+++ OK, passed 100 tests.
*Stack> quickCheck $ qcFoldl1 (+)
+++ OK, passed 100 tests.
*Stack> quickCheck $ qcFoldl1 (-)
*** Failed! Falsified (after 4 tests and 6 shrinks):    
0
[1]

-}

-- test with + - *
qcFoldr :: Eq b => (a -> b -> b) -> b -> [a] -> Bool
qcFoldr f z ls =
  l == c
  where
    s = genStack ls
    c = foldr f z ls
    l = stackFoldr f z s

{-

*Stack> quickCheck $ qcFoldr (+)
+++ OK, passed 100 tests.
*Stack> quickCheck $ qcFoldr (-)
+++ OK, passed 100 tests.
*Stack> quickCheck $ qcFoldr (*)
+++ OK, passed 100 tests.

-}


--commutative operations +, *
qcFoldr1 :: Eq b => (a -> b -> b) -> b -> [a] -> Bool
qcFoldr1 f z ls = l' ==
  l && c == l
  where
    s' = genStack $ reverse ls
    s = genStack ls
    c = foldr f z ls
    l = stackFoldr f z s
    l' = stackFoldr f z s'

{-
quickCheck $ \z ls -> qcFold (+) z ls
-}

{-
> quickCheck $ qcFoldl1 (+) 0
+++ OK, passed 100 tests.
> quickCheck $ qcFoldl1 (*) 0
+++ OK, passed 100 tests.
-}

qcZip :: [a] -> [b] -> Bool
qcZip ps qs =
  s == spq && s == sqp
    where
      p   = genStack ps
      q   = genStack qs
      pq  = stackZip p q
      spq = stackLen pq 0
      qp  = stackZip q p
      sqp = stackLen qp 0
      s   = minimum [length ps, length qs]

qcZipUnzip :: (Eq a1, Eq a2) => [a1] -> [a2] -> Bool
qcZipUnzip ls ms =
  gsl == l' && m' == gsm && m'' == gsm && l'' == gsl
  where
    sx =  if length ls <= length ms
          then length ls
          else length ms
    sl = take sx ls
    sm = take sx ms
    gsl = genStack sl
    gsm = genStack sm
    l = genStack ls
    m = genStack ms
    lm = stackZip l m
    ml = stackZip m l
    (m'', l'') = stackUnzip ml
    (l', m') = stackUnzip lm
    stackUnzip Empty = (Empty, Empty)
    stackUnzip (Stack s (u,v)) =  let (u',v') = stackUnzip s
                                  in (Stack u' u, Stack v' v)

-- > quickCheck $ qcZipUnzip
-- +++ OK, passed 100 tests.

qcMap :: Eq b => (a -> b) -> [a] -> Bool
qcMap f ls =
  genStack (map f ls) == stackMap f (genStack ls)

qcMapDiv :: Int -> [Int] -> Property
qcMapDiv d ls =
  d /= 0 ==> qcMap (`div` d) ls

{-
> quickCheck $ \n -> qcMap (n +)
+++ OK, passed 100 tests.
> quickCheck $ \n -> qcMap (n -)
+++ OK, passed 100 tests.
> quickCheck $ \n -> qcMap (n *)
+++ OK, passed 100 tests.
-}

-- length of stack remains same after map, zip
stackLen :: Stack a -> Int -> Int
stackLen Empty z       = z
stackLen (Stack s a) z = stackLen s (z+1)

-- inverse and length
qcMap1 :: Eq a => (a -> b) -> (b -> a) -> [a] -> Bool
qcMap1 f f' ls =
  length ls == m' && m' == lm && mx == s
  where
    s  = genStack ls
    m  = stackMap f s
    mx = stackMap f' m
    lm = stackLen s 0
    m' = stackLen m 0

-- > quickCheck $ qcLen (5 +) ((-5) +)
-- +++ OK, passed 100 tests.

-- maping with + 0 or * 1 results in the same stack (identity)

-- PROPERTY TEST --------------------------------------------E

propEmpty :: Eq b => (a -> b) -> Bool
propEmpty f = stackMap f Empty == Empty

-- Stack Test.QuickCheck> quickCheck (\n -> (propEmpty ((+) n)))

propOne :: Eq b => (a -> b) -> a -> Bool
propOne f x = stackMap f (Stack Empty x) == Stack Empty (f x)

-- Stack Test.QuickCheck> quickCheck (\n m -> (propOne ((*) n) m))

{-

propEmpty :: Eq b => (a -> b) -> Property 
propEmpty f = _ ==> stackMap f Empty == Empty

propOne :: Eq a1 => (a2 -> a1) -> a2 -> Property 
propOne f x = stackMap f (Stack Empty x) == Stack Empty (f x)

-}



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

myF :: (Integer -> Integer)
myF = (5 +)

emptyS :: Stack Integer
emptyS = Empty

oneS :: Stack Integer
oneS = Stack Empty 0

as :: Stack a
as = Empty
bs :: Stack Integer
bs = Stack as 1
cs :: Stack Integer
cs = Stack bs 2
xs :: Stack Integer
xs = Stack cs 3
ys :: Stack Integer
ys = Stack xs 4
zs :: Stack Integer
zs = Stack ys 5

twoS :: Stack Integer
twoS = Stack oneS 1

testMapN :: Stack Integer -> Stack Integer
testMapN = stackMap myF

testMapEmpty :: Stack Integer
testMapEmpty = stackMap myF emptyS

testMapOne :: Stack Integer
testMapOne = stackMap myF oneS

testFl1 :: Bool
testFl1 = stackFoldl (*) 6 zs == 720

testFl2 :: Bool
testFl2 = stackFoldl (^) 0 xs == 0

testFr1 :: Bool
testFr1 = stackFoldr (*) 6 zs == 720

testFr2 :: Bool
testFr2 = stackFoldr (^) 0 xs == 9

testMap :: Bool
testMap = stackFoldl (+) 0 (stackMap (+ 5) zs) == 40

testMap1 :: Bool
testMap1 =
  stackMap (2*) zs == Stack (Stack (Stack (Stack (Stack Empty 2) 4) 6) 8) 10

testZip :: Bool
testZip =
  stackZip xs ks == Stack (Stack (Stack Empty (1,6)) (2,8)) (3,10)
  where ks = stackMap (2*) zs

regTest :: Bool
regTest =
  testZip &&
  testMap1 && testMap &&
  testFr1 && testFr2 &&
  testFl1 && testFl2
