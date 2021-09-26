-- PROPERTY TEST --------------------------------------------S


-- Fold L
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

-- Fold R
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

Ok, one module loaded.
*Stack> quickCheck $ qcFoldr1 (+)
+++ OK, passed 100 tests.
*Stack> quickCheck $ qcFoldr1 (*)
+++ OK, passed 100 tests.
*Stack> quickCheck $ qcFoldr1 (-)
*** Failed! Falsified (after 4 tests and 6 shrinks):    
0
[0,1]

-}

-- Zip
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

{-

*Stack> quickCheck $ qcZip
+++ OK, passed 100 tests.

-}











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


-- Map
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
*Stack> quickCheck $ qcMapDiv 
+++ OK, passed 100 tests; 13 discarded.
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

{- apply f then apply inverse of f i.e. f'

*Stack> quickCheck $ \n -> qcMap1 (n +) ((-n) +)
+++ OK, passed 100 tests.

-}

-- more ideas
-- maping with (+ 0) or (* 1) results in the same stack (identity)

-- PROPERTY TEST --------------------------------------------E