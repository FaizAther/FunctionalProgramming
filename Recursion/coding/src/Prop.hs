module Prop (Prop, tautology) where

{--
You *MAY* use packages from base
https://hackage.haskell.org/package/base
but no others.

You may remove the comments if you like.

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

NOTE:  Feel free to create more functions.
--}

data Prop = Var String | And Prop Prop | Or Prop Prop | Not Prop deriving Show

tautology :: Prop -> Bool
tautology = undefined