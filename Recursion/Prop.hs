module Prop (Prop(..), tautology) where

{-

A Proposition is a expression which evaluates to a boolean.

Consider the following type for encoding a Proposition:

data Prop = Var String | And Prop Prop | Or Prop Prop | Not Prop deriving Show

The proposition "p and not q" would be encoded as
  And (Var "p") (Not $ Var "q")
and can be reduced by binding "p" and "q" to booleans
  And (Var "p") (Not $ Var "q") [p := True, q := False]
  -> And True (Not False)
   = And True True
   = True

A proposition which evaluates to True for any binding is called a TAUTOLOGY.

Implement a function which checks if a proposition is a tautology.

tautology :: Prop -> Bool
> tautology (Or (Var "p") (Not $ Var "p"))
True
> tautology (Var "p")
False

-}

data Prop = Var String
          | And Prop Prop
          | Or  Prop Prop
          | Not Prop
          deriving (Show, Eq)

genEntry :: Int -> ([Bool], [Bool])
genEntry  0 = ([False], [True])
genEntry  n = (l' ++ l', r' ++ r')
              where
                (l',r') = genEntry (n-1)

tableAdd :: [(String, [Bool])] -> String -> Int -> ([(String, [Bool])], Bool)
tableAdd []              s n =  ([(s, ls ++ rs)], True)
                                where
                                  (ls , rs) = genEntry n
tableAdd ((s', bs) : ts) s n
  | s == s'   = ((s', bs) : ts, False)
  | otherwise = if added then ((s', bs') : ts', added)
                else ((s', bs) : ts', added)
                where
                  (ts', added) = tableAdd ts s n
                  bs' = bs ++ bs

bind :: [(String, [Bool])] -> Prop -> ([(String, [Bool])], Prop)
bind t (Var s) =  (t', Var s)
                  where
                    (t', _) = tableAdd t s (length t)
bind t (Not p) = (t', Not p')
                 where
                   (t', p')  = bind t p
bind t (And p q) = (t'', And p' q')
                   where
                     (t', p')  = bind t  p
                     (t'', q') = bind t' q
bind t (Or p q) = (t'', Or p' q')
                  where
                    (t' , p')  = bind t  p
                    (t'', q')  = bind t' q

peekEntry :: String -> [(String, [Bool])] -> ([(String, [Bool])], Bool)
peekEntry v [(s, b:bs)]
  | v == s = ([(s, b:bs)], b)
peekEntry v ((s, b:bs) : ts)
  | v == s    = ((s, b:bs) : ts , b)
  | otherwise = ((s, b:bs) : ts', c)
                where
                  (ts', c) = peekEntry v ts

popEntry :: [(String, [Bool])] -> [(String, [Bool])]
popEntry []                 = []
popEntry ((s, [b]) : ts)    = []
popEntry ((s, b : bs) : ts) = (s, bs) : popEntry ts

eval :: [(String, [Bool])] -> Prop -> ([(String, [Bool])], Bool)
eval t (Var s) =  peekEntry s t
eval t (Not p) =  (t', not p')
                  where
                    (t', p') = eval t p
eval t (And p q) =  (t'', p' && q')
                    where
                      (t', p') = eval t p
                      (t'', q') = eval t' q
eval t (Or p q) = (t'', p' || q')
                  where
                    (t', p') = eval t p
                    (t'', q') = eval t' q


andAll :: Prop -> [(String, [Bool])] -> Bool
andAll p [] = True
andAll p t  = b && andAll p t'
              where
                (_, b) = eval t p
                t' = popEntry t

tautology :: Prop -> Bool
tautology p = andAll p t
              where
                (t, _) = bind [] p
