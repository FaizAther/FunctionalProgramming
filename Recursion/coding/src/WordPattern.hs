module WordPattern (wordPattern) where

{--
You *MAY* use packages from base
https://hackage.haskell.org/package/base
but no others.

You may remove the comments if you like.

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

wordPattern :: String -> [String] -> Bool
wordPattern = undefined