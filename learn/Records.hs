

--  Person Data Type
--  Person <Name> <Age>
--
data Person = Person String Int
  deriving Show

data BetterPerson = BetterPerson { name :: String,
                                   age  :: Int }
  deriving Show


greet :: BetterPerson -> [Char]
greet (BetterPerson name _) = "hi " ++ name

data Point =
    D2 { x :: Int, y :: Int }
  | D3 { x :: Int, y :: Int, z :: Int }
  deriving (Show, Eq)

