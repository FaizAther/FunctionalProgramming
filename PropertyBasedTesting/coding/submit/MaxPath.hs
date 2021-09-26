module MaxPath where

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