module WordPattern (wordPattern) where

import Test.QuickCheck (Property, (==>))

{--
Given a pattern ps and a word list wss, determine if the word list follows the 
same pattern.

That is to say, 
    ps !! j == ps !! k 
only when 
    wss !! j == wss !! k

And
    ps !! j /= ps !! k
means
    wss !! j /= wss k
--}


tableAdd :: Char -> String -> [(Char, String)] -> ([(Char, String)], Bool)
tableAdd c s []     = ([(c,s)], True)
tableAdd c s ((c', s'):xs)
  | c == c' = ((c', s'):xs, s == s')
  | s == s' = ((c', s'):xs, c == c')
  | otherwise = let (xs', r) = tableAdd c s xs
                in ((c', s'):xs', r)

tableAddH :: String  -> [String] -> [(Char, String)] -> ([(Char, String)], Bool)
tableAddH []      []   t = (t, True)
tableAddH _       []   t = (t, False)
tableAddH []      _    t = (t, False)
tableAddH (c:cs) (s:ss) t = if not check then
                            (t', check) else
                            tableAddH cs ss t'
                            where
                              (t', check) = tableAdd c s t


wordPattern :: String -> [String] -> Bool

wordPattern cs ss = let (t, r) = tableAddH cs ss []
                    in r
{--
=======
EXAMPLE
=======

> wordPattern "aaaa" ["cat", "cat", "cat", "cat"]
True

> wordPattern "aaaa" ["dog", "cat", "cat", "cat"]
False

> wordPattern "xy" ["cat", "cat"]
False

> wordPattern "xoox" ["dog", "cat", "cat", "dog"]
True

> wordPattern "xooy" ["dog", "cat", "cat", "fish"]
True

--}

prop :: [Char] -> [String] -> Property
prop ps wss =
    (length ps == length wss)
        ==> wordPattern ps wss
            ==  (
                  length  ps == length wss
                  && and [(ps !! j) /= (ps !! k) || (wss !! j == wss !! k) | j <- idxs, k <- idxs]
                  && and [(ps !! j) == (ps !! k) || (wss !! j /= wss !! k) | j <- idxs, k <- idxs]
                )
        where
            idxs = [0..(length ps-1)]
