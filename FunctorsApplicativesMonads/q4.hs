module Q4 where


data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
            deriving Show


instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Node lt x rt) = Node (fmap f lt) (f x) (fmap f rt)

instance Applicative Tree where
  pure x = Leaf x
  (Leaf fs) <*> (Leaf x) = Leaf $ fs x
  (Node l f r) <*> (Leaf x) = undefined 

{-
  Base Case
  -------
  fmap (g . h) (Leaf x)
  = Leaf $ (g . h) x             [apply fmap def.]
  = Leaf $ (g (h x))             [apply (.) def.]
  = fmap g (Leaf $ h x)          [unapply fmap g]
  = fmap h (fmap g (Leaf $ h x)) [unapply fmap h]
  = fmap g . fmap h (Leaf $ x)   [unapply (.) def.]

  I.H
  -------
  Assume holds for lt and rt in (Node lt x rt)

  fmap (g . h) lt = fmap g . fmap h lt
  fmap (g . h) rt = fmap g . fmap h rt

  I.
  -------
  Prove for fmap (g . h) (Node lt x rt) = fmap g . fmap h (Node lt x rt)

  fmap (g . h) (Node lt x rt)
  = Node (fmap (g . h) lt) ((g . h) x) (fmap (g . h) rt)        [ apply def.       ]
  = Node (fmap g . fmap h lt) ((g . h) x) (fmap g . fmap h rt)  [ by I.H           ]
  = Node (fmap g (fmap h lt)) (g (h x)) (fmap g (fmap h rt))    [ apply (.) def.   ]
  = fmap g (Node (fmap h lt) (h x) (fmap h rt))                 [ unapply fmap g   ]
  = fmap g (fmap h (Node lt x rt))                              [ unapply fmap h   ]
  = fmap g . fmap h (Node lt x rt)                              [ unapply (.) def. ]
-}