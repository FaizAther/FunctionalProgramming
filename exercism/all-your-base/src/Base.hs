module Base (Error(..), rebase, fromBase, toBase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

-- | A number transformed from another base, where a number is represented 
-- | as a sequence of decimal numbers respresenting 'digits' in the base of 
-- | that number.
-- | NB. 
-- | [1] A 'null' input number of `[]` returns `[]`, since no `Error` is 
-- |     defined for this condition.
-- |
-- | [2] Bizarely , a input number that is equal to zero (e.g. if it comes 
-- |     from `[0]` or `[0,0]`) is supposed to be returned as `[]` according 
-- |     to the test suite.
--
rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits

    | inputBase  < 2   = Left InvalidInputBase
    | outputBase < 2   = Left InvalidOutputBase
    | null inputDigits = Right []

    | otherwise = do
        decNum <- fromBase inputBase inputDigits
        if decNum == 0
            then Right [] -- (bizare special requirement).
            else toBase outputBase decNum

-- | The base-10 number converted from a number in a given base where 
-- | each 'digit' in that number is represented by a decimal number.
-- | NB. An empty input list of digits returns `0`, since no `Error` 
-- | is defined for this condition.
--
-- -- Alternative: --
-- fromBase :: Integral a => a -> [a] -> Either (Error a) a
-- fromBase inputBase inputDigits
--     | inputBase < 2 = Left InvalidInputBase
--     | otherwise     = foldl go (Right 0) inputDigits
--     where
--      -- go :: Integral a => Either (Error a) a -> a -> Either (Error a) a
--         go err@(Left _)    _ = err
--         go     (Right acc) d = if d < 0 || d >= inputBase
--                                 then Left  $ InvalidDigit d
--                                 else Right $ acc * inputBase + d
fromBase' :: Integral a => a -> [a] -> Either (Error a) a
fromBase' inputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | otherwise     = go (Right 0) inputBase inputDigits
    where
        go :: Integral a => Either (Error a) a -> a -> [a] -> Either (Error a) a
        go acc               _ []     = acc
        go acc@(Left  _    ) _ _      = acc
        go     (Right tally) b (x:xs) = if x < 0 || x >= b
                                            then Left $ InvalidDigit x
                                            else go (Right (tally * b + x)) b xs

checkBase :: (Ord a, Num a) => a -> Bool
checkBase b = b < 2

checkFold :: (Ord a, Num a) => a -> a -> Bool
checkFold b n = n >= 0 && n < b

myF :: Num a => a -> a -> a -> a
myF acc b n = acc * b + n

doFold' :: (Ord a, Num a) =>a -> Either (Error a) a -> a -> Either (Error a) a
doFold' b (Left e) n = Left e
doFold' b (Right acc) n
    | checkFold b n = Right $ myF acc b n
    | otherwise = Left $ InvalidDigit n

fromBase'' :: Integral a => a -> [a] -> Either (Error a) a
fromBase'' b ls
    | checkBase b = Left InvalidInputBase
    | otherwise   = foldl (doFold' b) (Right 0) ls

doFold'' b (Left e) n = Left e
doFold'' b (Right acc) n
    | checkFold b n = Right $ myF acc b n
    | otherwise = Left $ InvalidDigit n

fixBase :: [a] -> [Either (Error a) a]
fixBase = map (\a -> Right a >>= doFold')

fromBase :: (Ord a, Num a, Traversable t) => a -> t a -> Either (Error a) a
fromBase b ls
    | checkBase b = Left InvalidInputBase
    | otherwise   = _

-- | The number in a given base (where each 'digit' in that number is 
-- | represented by a decimal number) converted from a given non-negative 
-- | base-10 number.
--
toBase :: Integral a => a -> a -> Either (Error a) [a]
toBase outputBase decNum
    | decNum < 0     = undefined
    | outputBase < 2 = Left InvalidOutputBase
    | otherwise      = Right $ go [] decNum
    where
     -- go :: Integral a => [a] -> a -> [a]
        go acc n =
            let
                (quotient, remainder) = divMod n outputBase
                acc' = remainder : acc
            in
                if quotient == 0
                    then acc'
                    else go acc' quotient