
data Color = Color { redC :: Int, greenC :: Int
                   , blueC  :: Int, alphaC :: Int
                   } deriving (Show, Eq)

instance Semigroup Color where
  Color r1 g1 b1 a1 <> Color r2 g2 b2 a2
          = Color (mix r1 r2)
                  (mix g1 g2)
                  (mix b1 b2)
                  (mix a1 a2)
        where
          mix x1 x2 = min 255 (x1 + x2)

instance Monoid Color where
  mempty = Color 0 0 0 0


newtype Score = S Integer

instance Semigroup Score where
  S x <> S y = S (x + y)

instance Monoid Score where
  mempty = S 0
