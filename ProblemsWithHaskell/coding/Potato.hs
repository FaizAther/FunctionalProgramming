module Potato where

--- do not change anything above this line --- 

{--
You *MAY* use packages from base
https://hackage.haskell.org/package/base
but no others.

During a game of "hot potato" a token (a potato) is passed amongst a group of 
ten people with labels 0, 1, 2, ..., 9 until some timer goes off at which point 
the game ends.

Suppose a game of "hot potato" is played among people with unique Integer
identifiers. So, for instance, a game with the following moves
(0, 1) -- Person 0 passes potato to Person 1
(1, 2) -- Person 1 passes potato to Person 2
(2, 3) -- Person 2 passes potato to Person 3
(3, 4) -- Person 3 passes potato to Person 4
(4, 5) -- Person 4 passes potato to Person 5 and game ends.
can be ENCODED as
[0, 1, 2, 3, 4, 5]

You can also have more sophisticated games where the potato is passed back 
to people who have already had the potato:
(0, 2)
(2, 0)
(0, 1)
(1, 2)
(2, 1)
which gets ENCODED as
[0, 2, 0, 1, 2, 1]

ASSUME: Person 0 ALWAYS initially has the potato and write a function
    encode :: [(Int, Int)] -> Maybe [Int]
that given a list of passes made during a game, returns the encoded game.
    > encode [(0, 2), (2, 0), (0, 1), (1, 2), (2, 0)]
    Just [0, 1, 2, 0, 2, 0]
otherwise, if the tuples correspond to an impossible game, return Nothing.
    > encode [(0, 2), (2, 0), (1, 2)]
    Nothing

Q:  Why is this hard?
A:  The list of tuples will not be given in order.

    > encode [(4,5),(1,2),(2,3),(0,1),(3,4)]
    Just [0, 1, 2, 3, 4, 5]
    
    > encode [(0, 1), (0, 2), (1, 2), (2, 0), (2, 1)]
    Just [0, 1, 2, 0, 2, 1]

When there are MULTIPLE CORRECT ENCODINGS.  Return the encoding that is LEAST
when converted into a base ten number.

    > encode [(0, 1), (0, 2), (1, 2), (2, 0), (2, 1)]
    Just [0, 1, 2, 0, 2, 1]
    -- even though [0, 2, 1, 2, 0, 1] is also a valid game, 12021 < 21201 and 
    thus the former is returned.

Notes:
1.  Base case:
    > encode []
    Just [0]

2.  You *must* use *every* tuple in the input list to form the game (if 
    possible).

3.  Assume the tuples are taken from [(x,y) | x <- [0..9], y <- [0..9]].  If
    you encouter a tuple not from this list then return Nothing.
    > encode [(0,1),(1,17)]
    Nothing
--}

checkN :: Int -> Bool
checkN n = n >= 0 && n <= 9

checkTuple :: (Int, Int) -> Bool
checkTuple (f, t) = checkN f && checkN t

insertL :: [Int] -> Int -> [Int]
insertL [] x = [x]
insertL (l:ls) x
  | x <= l = x:l:ls
  | otherwise = l : insertL ls x

convertList :: [(Int, Int)] -> [(Int, [Int])] -> [(Int, [Int])]
convertList [] ls = ls
convertList ( (from, to) : xs) [] = convertList xs [(from, [to])]
convertList ( (from, to) : xs) ( (fs, ts) : ys)
  | from == fs = convertList xs ((fs, insertL ts to) : ys)
  | otherwise = (fs, ts) : convertList ((from, to) : xs) ys

traverseL :: [(Int, [Int])] -> Int -> Maybe [Int] -> Maybe [Int]
traverseL _  se Nothing   = Nothing
traverseL xs  (-1) j = if checkDone xs then j else Nothing
traverseL [] se (Just ns) = Just ns
traverseL xs se (Just ns) = let (b, i, t') = traverseL' xs se
                            in  if b then traverseL t' i (Just (i:ns))
                                else if not b && checkDone t' then Just (i:ns)
                                else Nothing

checkDone :: [(Int, [Int])] -> Bool
checkDone [] = True
checkDone ((f,[]):xs) = checkDone xs
checkDone ((f,t:ts):xs) = False

traverseL' :: [(Int, [Int])] -> Int -> (Bool, Int, [(Int, [Int])])
traverseL' [] se = (False, se, [])
traverseL' ((f, []):xs) se
  | se == f = (True, -1, (f, []):xs)
  | otherwise = let (b, i, xs') = traverseL' xs se in (b, i, (f, []):xs')
traverseL' ((f, t:ts):xs) se
  | se == f = (True, t, (f, ts):xs)
  | otherwise = let (b, i, xs') = traverseL' xs se in (b, i, (f, t:ts):xs')

check1 :: [(Int,Int)]
check1 = [(0, 1), (0, 2), (1, 2), (2, 0), (2, 1)]

check2 ::Bool 
check2 = encode [(0, 1), (0, 2), (1, 2), (2, 0), (2, 1)] ==
  Just [0, 1, 2, 0, 2, 1]

check3 :: Bool 
check3 = encode [(0, 2), (2, 0), (1, 2)] == Nothing 

check4 :: Bool
check4 = encode [(4,5),(1,2),(2,3),(0,1),(3,4)] ==
    Just [0, 1, 2, 3, 4, 5]

encode :: [(Int, Int)] -> Maybe [Int]
encode xs = if checkXS xs then reverse . tail' <$> traverseL (convertList (qsort xs) []) 0 (Just [0]) else Nothing

checkXS :: Foldable t => t (Int, Int) -> Bool
checkXS = foldr ((&&) . checkTuple) True

tail' :: [p] -> [p]
tail' [x] = [x]
tail' (x:xs) = xs

qsort :: [(Int, Int)] -> [(Int, Int)]
qsort []     = []
qsort (x:xs) = qsort lower ++
               [pivot] ++
               qsort upper
  where
    pivot = x
    lower = [y | y <- xs, y `lessEqC` x]
    upper = [y | y <- xs, y `greaterC`  x]

lessEqC :: (Int, Int) -> (Int, Int) -> Bool
lessEqC (x,_) (y,_) = x <= y

greaterC :: (Int, Int) -> (Int, Int) -> Bool
greaterC x y = not $ lessEqC x y