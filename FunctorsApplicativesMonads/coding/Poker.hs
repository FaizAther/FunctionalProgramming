module Poker where

--- do not change anything above this line --- 

{--
You *MAY* use packages from base
https://hackage.haskell.org/package/base
but no others.

Suppose the following datatype for representing a standard deck of cards and
    data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Ord)
    data Rank = Numeric Int | Jack | Queens | King | Ace deriving (Eq, Ord)
    data Card = NormalCard Rank Suit | Joker deriving Eq

Your task is to determine the HAND RANKING of hand :: List[Card].  The HAND
RANKINGS in DESCENDING ORDER is given by:
    FiOAK -- five of a kind
    StFl  -- straight flush
    FoOAK -- four of a kind
    FuHo  -- full house
    Fl    -- flush
    St    -- straight
    TrOAK -- three of a kind
    TwPr  -- two pair
    OnPr  -- one pair
    HiCa  -- high card
where the definitions of the above are here: en.wikipedia.org/wiki/List_of_poker_hands
(Ignore the (**) note that says "Category does not exist under ace-to-five low rules")

Supposing
    data HandRanking = FiOAK | StFl | FoOAK | FuHo | Fl | St | TrOAK | TwPr | OnPr | HiCa deriving (Show, Eq, Ord)
write a function 
    ranking :: (Card, Card, Card, Card, Card) -> HandRanking
that returns the GREATEST ranking among a hand of cards.


NOTES:

1/  Do *not* assume hands are drawn from a standard deck.  That is, presume any 
    card can appear in duplicate.  In particular, assume any hand can have an 
    arbitrary numbers of jokers in it.

2/  An Ace can be considered to have numeric rank 1 for the purposes of 
    forming a straight or straight flush.

EXAMPLE
> ranking (Joker, Joker, Joker, Joker, Joker)
FiOAK

> ranking ((NormalCard Ace Hearts),
    (NormalCard (Numeric 2) Hearts),
    (NormalCard (Numeric 3) Hearts),
    (NormalCard (Numeric 4) Hearts),
    (NormalCard (Numeric 5) Hearts))
StFl

> ranking (Joker,
    (NormalCard (Numeric 2) Hearts),
    (NormalCard (Numeric 2) Spades),
    (NormalCard (Numeric 3) Hearts),
    Joker)
FoOAK
-- NOT FuHo because Full House has lower rank.
--}

data Suit = Hearts
          | Clubs
          | Diamonds
          | Spades
          deriving (Show, Eq)

data Rank = Numeric Int
          | Jack
          | Queen
          | King
          | Ace
          deriving (Show, Eq, Ord)

data Card = NormalCard Rank Suit
          | Joker
          deriving (Show)

data HandRanking  = HiCa
                  | OnPr
                  | TwPr
                  | TrOAK
                  | St
                  | Fl
                  | FuHo
                  | FoOAK
                  | StFl
                  | FiOAK
                  deriving (Show, Eq, Ord)


instance Eq Card where
  --(==) :: Card -> Card -> Bool
  Joker == _ = True
  _ == Joker = True
  NormalCard r1 s1 == NormalCard r2 s2 = r1 == r2 && s1 == s2

sameRank ::  Card -> Card -> Bool
Joker `sameRank` _ = False
_ `sameRank` Joker = False
NormalCard r1 s1 `sameRank` NormalCard r2 s2 = r1 == r2

sameSuit :: Card -> Card -> Bool
Joker `sameSuit` _ = False
_ `sameSuit` Joker = False
NormalCard r1 s1 `sameSuit` NormalCard r2 s2 = s1 == s2

sameJoker :: Card -> Card -> Bool
sameJoker Joker Joker = True
sameJoker _ _ = False

insertCardP ::  (Card -> Card -> Bool) ->
                [[(Int, Card)]] ->
                [(Int, Card)] ->
                [[(Int, Card)]]
insertCardP p cs []              =  cs
insertCardP p cs ((_,Joker):cns) =  insertCardP p cs cns
insertCardP p [[]] (cn:cns)      =  let cs' = [[cn]]
                                    in insertCardP p cs' cns
insertCardP p [] (cn:cns)        =  let cs' = [[cn]]
                                    in insertCardP p cs' cns
insertCardP p (cs:css) (cn:cns)  =  let (cs',b) = insertCardP' p cs cn
                                    in  if b then insertCardP p (cs':css) cns
                                        else insertCardP p (cs : insertCardP p css [cn]) cns
--insertSuit x y = error ("cs are: " ++ show x ++ "\ncns are : " ++ show y)

insertCardP' :: (Card -> Card -> Bool) -> [(Int, Card)] -> (Int, Card) -> ([(Int, Card)], Bool)
insertCardP' p ((nx,cx):cxs) (n,c)
  | nx /= n && p cx c = ((n,c):(nx,cx):cxs, True)
  | otherwise = ((nx,cx):cxs, False)

testcs :: [(Int, Card)]
testcs = toList (NormalCard Jack Spades,
                  NormalCard Jack Hearts,
                  NormalCard (Numeric 1) Hearts,
                  NormalCard (Numeric 1) Hearts,
                  NormalCard Queen Spades)

testcs1 :: [(Int, Card)]
testcs1 = toList (Joker,
                  NormalCard Jack Hearts,
                  NormalCard (Numeric 1) Hearts,
                  Joker,
                  NormalCard Queen Spades)

myFold :: (Card -> Card -> Bool) -> [(Int, Card)] -> [[(Int, Card)]]
myFold p = insertCardP p [[]]

keepJokers :: [(Int, Card)] -> [(Int, Card)]
keepJokers [] = []
keepJokers ((n, Joker):ls) = (n,Joker) : keepJokers ls
keepJokers (_:ls) = keepJokers ls

sortCards ::  [(Int, Card)] ->
              ([[(Int, Card)]], [[(Int, Card)]], [(Int, Card)], [(Int, Card)])
sortCards cs = (myFold sameSuit cs,myFold sameRank cs,keepJokers cs, cs)


showCs :: ([[(Int, Card)]], [[(Int, Card)]], [(Int, Card)], [(Int, Card)]) -> String
showCs (suits, ranks, jokers, orig) = unlines ["Suits: " ++ show suits,
                                      "Ranks: " ++ show ranks,
                                      "Joker: " ++ show jokers,
                                      "Orig:  " ++ show orig]

handRankList :: [HandRanking]
handRankList = [FiOAK,StFl,FoOAK,FuHo,Fl,St,TrOAK,TwPr,OnPr,HiCa]

isRanking :: HandRanking ->
             ([[(Int, Card)]], [[(Int, Card)]], [(Int, Card)], [(Int, Card)]) ->
             Bool
isRanking HiCa  (ss, rs, js, os)  = True
isRanking OnPr  (ss, rs, js, os)  =
  not err && (lj'' >= 0)
  where
    lj' = lj - ls1'
    lj'' = lj' - ls2'
    err = ls1' < 0 || ls2' < 0
    ls1' = 2 - ls1
    ls2' = 1 - ls2
    lj  = length js
    ls1 = head rs'
    ls2 = head $ tail rs'
    rs' = qsortN $ map length rs
isRanking TwPr  (ss, rs, js, os)  =
  not err && (lj'' >= 0)
  where
    lj' = lj - ls1'
    lj'' = lj' - ls2'
    err = ls1' < 0 || ls2' < 0
    ls1' = 2 - ls1
    ls2' = 2 - ls2
    lj  = length js
    ls1 = head rs'
    ls2 = head $ tail rs'
    rs' = qsortN $ map length rs
isRanking TrOAK (ss, rs, js, os)  =
  (lr + lj) == 3
  where
    lj = length js
    lr = (head . qsortN) (map length rs)
isRanking St    (ss, rs, js, os)  = checkStraight os
isRanking Fl    (ss, rs, js, os)  =
    (lr + lj) == 3
  where
    lj = length js
    lr = (head . qsortN) (map length ss)
isRanking FuHo  (ss, rs, js, os)  =
  not err && (lj'' >= 0)
  where
    lj' = lj - ls1'
    lj'' = lj' - ls2'
    err = ls1' < 0 || ls2' < 0
    ls1' = 3 - ls1
    ls2' = 2 - ls2
    lj  = length js
    ls1 = head rs'
    ls2 = head $ tail rs'
    rs' = qsortN $ map length rs
isRanking FoOAK (ss, rs, js, os)  =
  (lr + lj) == 4
  where
    lj = length js
    lr = (head . qsortN) (map length rs)
isRanking StFl  (ss, rs, js, os)  =
  (ml+jl) >= 5 && checkStraight os
  where
    jl = length js
    ml = (head . qsortN) (map length ss)
isRanking FiOAK (ss, rs, js, os)  =
  (lr + lj) >= 5
  where
    lj = length js
    lr = (head . qsortN) (map length rs)

onprCards1 :: [(Int, Card)]
onprCards1 = toList (
    NormalCard (Numeric 10) Spades,
    NormalCard (Numeric 10) Hearts,
    NormalCard (Numeric 8) Spades,
    NormalCard (Numeric 7) Hearts,
    NormalCard (Numeric 4) Clubs)

twprCards1 :: [(Int, Card)]
twprCards1 = toList (
    NormalCard Jack Hearts,
    NormalCard Jack Spades,
    NormalCard (Numeric 3) Clubs,
    NormalCard (Numeric 3) Spades,
    NormalCard (Numeric 2) Hearts)

troakCards1 :: [(Int, Card)]
troakCards1 = toList (
    NormalCard Jack Hearts,
    NormalCard Queen Hearts,
    NormalCard Queen Hearts,
    NormalCard Queen Hearts,
    NormalCard (Numeric 3) Hearts)

flCards1 :: [(Int, Card)]
flCards1 = toList (
    NormalCard Jack Hearts,
    NormalCard (Numeric 9) Hearts,
    NormalCard (Numeric 8) Hearts,
    NormalCard (Numeric 4) Hearts,
    NormalCard (Numeric 3) Hearts)

fuhoCards1 :: [(Int, Card)]
fuhoCards1 = toList (
    NormalCard Ace Hearts,
    NormalCard King Diamonds,
    NormalCard Ace Spades,
    NormalCard Ace Clubs,
    Joker)

fooakCards1 :: [(Int, Card)]
fooakCards1 = toList (
    NormalCard Ace Hearts,
    NormalCard King Diamonds,
    NormalCard Ace Spades,
    NormalCard Ace Clubs,
    Joker)

--Todo
checkStraight :: [(Int, Card)] -> Bool
checkStraight [] = True
checkStraight [(n1,c1)] = True
checkStraight ((n1,c1):(n2,c2):cs) = c1 <= c2 && nextCard c1 c2

nextCard :: Card -> Card -> Bool 
nextCard Joker _ = True
nextCard _ Joker = True
nextCard (NormalCard Ace _) (NormalCard (Numeric 1) _) = True
nextCard (NormalCard (Numeric 5) _) (NormalCard Ace _) = True
nextCard (NormalCard (Numeric x) _) (NormalCard (Numeric y) _) = (x + 1) == y
nextCard (NormalCard (Numeric 10) _) (NormalCard Jack _) = True
nextCard (NormalCard Jack _) (NormalCard Queen _) = True
nextCard (NormalCard Queen _) (NormalCard King _) = True
nextCard (NormalCard King _) (NormalCard Ace _) = True
nextCard _ _ = False  


cStr :: ([[(Int, Card)]], [[(Int, Card)]], [(Int, Card)], [(Int, Card)])
cStr = sortCards $ toList (NormalCard Ace Spades,
                  NormalCard King Hearts,
                  NormalCard Queen Hearts,
                  NormalCard Jack Hearts,
                  NormalCard (Numeric 10) Hearts)

instance Ord Card where
  (<=) Joker _ = True
  (<=) _ Joker = True
  (<=) (NormalCard r1 _) (NormalCard r2 _) = r1 <= r2


fioakCards1 :: [(Int, Card)]
fioakCards1 = toList (
    NormalCard Ace Hearts,
    NormalCard Ace Diamonds,
    NormalCard Ace Spades,
    NormalCard Ace Clubs,
    Joker)

checkCards :: [(Int,Card)] -> HandRanking -> Bool
checkCards cs hr = isRanking hr $ sortCards cs

toList :: (Card, Card, Card, Card, Card) -> [(Int, Card)]
toList (c1,c2,c3,c4,c5) = qsort [(1,c1),(2,c2),(3,c3),(4,c4),(5,c5)]

lessEqC :: (Int, Card) -> (Int, Card) -> Bool
lessEqC (n, NormalCard r1 s1) (m, NormalCard r2 s2) = r1 <= r2
lessEqC (n, c) (m, d) = c <= d

greaterC :: (Int, Card) -> (Int, Card) -> Bool
greaterC (n, NormalCard r1 s1) (m, NormalCard r2 s2) = r1 > r2
greaterC (n, c) (m, d) = c > d

qsort :: [(Int, Card)] -> [(Int, Card)]
qsort []     = []
qsort (x:xs) = qsort lower ++
               [pivot] ++
               qsort upper
  where
    pivot = x
    lower = [y | y <- xs, y `lessEqC` x]
    upper = [y | y <- xs, y `greaterC`  x]

qsortN :: Ord a => [a] -> [a]
qsortN []     = []
qsortN (x:xs) = qsortN lower ++
                [pivot] ++
                qsortN upper
  where
    pivot = x
    lower = [y | y <- xs, y >= x]
    upper = [y | y <- xs, y <  x]

ranking :: (Card, Card, Card, Card, Card) -> HandRanking
ranking cs = 
  getIt hs' 
  where
    cs' = toList cs
    p :: HandRanking -> Bool
    p = checkCards cs'
    hs = handRankList
    hs' = map (\hr -> let b = p hr in (b,hr)) hs
    getIt [] = HiCa
    getIt ((b,hr):ms) = if b then hr else getIt ms

check1 :: Bool
check1 = ranking (Joker, Joker, Joker, Joker, Joker) ==
  FiOAK

check2 :: Bool
check2 = ranking (NormalCard Ace Hearts,
    NormalCard (Numeric 2) Hearts,
    NormalCard (Numeric 3) Hearts,
    NormalCard (Numeric 4) Hearts,
    NormalCard (Numeric 5) Hearts) ==
  StFl

check3 :: Bool
check3 = ranking (Joker,
    NormalCard (Numeric 2) Hearts,
    NormalCard (Numeric 2) Spades,
    NormalCard (Numeric 3) Hearts,
    Joker) ==
  FoOAK


-- ----------------------------- joker testing tests:

check4 :: Bool
check4 = ranking (NormalCard (Numeric 2) Spades,
                  NormalCard (Numeric 2) Hearts,
                  NormalCard (Numeric 3) Spades,
                  NormalCard (Numeric 3) Hearts,
                  Joker) ==
  FuHo

check5 :: Bool
check5 = ranking (Joker,
                  NormalCard (Numeric 2) Hearts,
                  NormalCard (Numeric 3) Hearts,
                  NormalCard (Numeric 4) Hearts,
                  NormalCard (Numeric 5) Hearts) ==
  StFl

check6 :: Bool
check6 = ranking (Joker,
                  Joker,
                  NormalCard (Numeric 3) Hearts,
                  NormalCard (Numeric 4) Hearts,
                  NormalCard (Numeric 5) Hearts) ==
  StFl

check7 :: Bool
check7 = ranking (Joker,
                  Joker,
                  Joker,
                  NormalCard (Numeric 4) Hearts,
                  NormalCard (Numeric 5) Hearts) ==
  StFl

check8 :: Bool
check8 = ranking (Joker,
                  Joker,
                  Joker,
                  Joker,
                  NormalCard (Numeric 5) Hearts) ==
  FiOAK

-- check that Jokers can morph to bridge a stFl
check9 :: Bool
check9 = ranking (NormalCard (Numeric 2) Hearts,
                  Joker,
                  Joker,
                  Joker,
                  NormalCard (Numeric 6) Hearts) ==
  StFl

-- check that Jokers can't morph to bridge an un-bridgeable strFl
check10 :: Bool
check10 = ranking (NormalCard (Numeric 2) Spades,
                  Joker,
                  Joker,
                  Joker,
                  NormalCard (Numeric 7) Hearts) ==
  FoOAK

-- --------------------------------------------------

-- ---------------------------- straight tests:
-- broadway straight
check11 :: Bool
check11 = ranking (NormalCard Ace Spades,
                  NormalCard (Numeric 2) Hearts,
                  NormalCard (Numeric 3) Hearts,
                  NormalCard (Numeric 4) Hearts,
                  NormalCard (Numeric 5) Hearts) ==
  St

-- wheel straight
check12 :: Bool
check12 = ranking (NormalCard Ace Spades,
                  NormalCard King Hearts,
                  NormalCard Queen Hearts,
                  NormalCard Jack Hearts,
                  NormalCard (Numeric 10) Hearts) ==
  St

-- --------------------------------------------------
checkAll :: Bool
checkAll =
  check1 &&
  check2 &&
  check3 &&
  check4 &&
  check5 &&
  check6 &&
  check7 &&
  check8 &&
  check9 &&
  check10 &&
  check11 &&
  check12