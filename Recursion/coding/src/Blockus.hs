module Blockus (tile) where

{-
You *MAY* use packages from base
https://hackage.haskell.org/package/base
but no others.

You may remove the comments if you like.

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

tile :: Int -> [[Int]]
tile = undefined