

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Show)



treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = (Leaf (f a))
treeMap f (Branch l r) = Branch (treeMap f l) (treeMap f r)


class Functor f where
  fmap :: (a -> b) -> f a -> f b


--fmap :: (a -> b) -> Maybe a -> Maybe b

instance Main.Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)
