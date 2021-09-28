module Q3 where


import Prelude hiding ((<$>), (<*>), pure)

(<$>) :: (a -> b) -> [a] -> [b]
_ <$> []     = []
f <$> (x:xs) = f x : (f <$> xs)

pure :: a -> [a]
pure a = [a]

(<*>) :: [a -> b] -> [a] -> [b]
[] <*> _ = []
_ <*> [] = []
(f:fs) <*> (x:xs) = (f <$> pure x) ++ (fs <*> xs)

prop :: Eq b => [a -> b] -> a -> Bool
prop x y = x <*> pure y == pure ($ y) <*> x

prop1 :: Bool
prop1 = (\x y z -> x + y + z) <$> [1,2,3,10] <*> [4,5,6,10,11] <*> [7] == [12]

newtype ZipList a = Z [a] deriving Show
{-
xs :: ZipList Integer
xs = Z [1,2,3]

instance Functor ZipList where
    fmap _ (Z [])     = Z []
    fmap f (Z (x:xs)) = Z $ f x : (f <$> xs)

instance Applicative ZipList where
    -- pure :: a -> Z [a]
    pure a = Z [a]

    -- (<*>) :: [a -> b] -> [a] -> [b]
    Z [] <*> _ = Z []
    _ <*> Z [] = Z []
    Z (f:fs) <*> (Z (x:xs)) = Z $ pure (f x) ++ (fs <*> xs)
-}
{-
  assuming x = [x'].

  Note : (\g -> g y) = ($ y)

  x <*> pure y                          [ by assumption    ]
  = [x'] <*> pure y                     [ apply pure       ]
  = [x'] <*> [y]                        [ apply <*>        ]
  = (x' <$> [y]) ++ ([] <*> [])         [ apply <*>        ]
  = (x' <$> [y]) ++ []                  [ apply ++         ]
  = (x' <$> pure y)                     [ apply pure       ]
  = (x' <$> [y])                        [ apply <$>        ]
  = (x' $ y) : (x' <$> [])              [ apply <$>        ]
  = (x' $ y) : []                       [ apply (:)        ]
  = [(x' $ y)]                          [ unapply ($ y)    ]
  = [(($ y) x')]                        [ unapply <$>      ]
  = [($ y)] <$> [x']                    [ unapply <*>      ]
  = pure ($ y) <*> [x']                 [ note             ]
  = pure (\g -> g y) <*> [x']           [ by assumption    ]
  = pure (\g -> g y) <*> x
-}
