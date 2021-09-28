-- The University of Queensland
-- Ather, Mohammad Faiz
-- COMP3400 2021
-- Assignment 2
-- Question 4

-- Piano Numbers

--Using induction over n prove that

-- emb( times n m ) = emb(n) * emb(m)

data Nat = Zero | Succ Nat deriving Show

emb :: Nat -> Int
emb Zero     = 0
emb (Succ n) = 1 + emb n

plus :: Nat -> Nat -> Nat
plus n Zero     = n
plus n (Succ m) = plus (Succ n) m

times :: Nat -> Nat -> Nat
times _ Zero = Zero
times n (Succ m) = plus n $ times n m

{-  
  1) plus
  --------
  
  Base Case
  ---------
  emb( plus Zero Zero )   
  == emb( Zero )
  == 0
  == 0 + 0
  == emb( Zero ) + emb( Zero )

  I.H
  ---------Assume holds for n, m

  emb( plus n m )   == emb( n ) + emb( m ) 

  I.
  ----------Prove true for (n), (Succ m)

  emb( plus n (Succ m) )          [ def. emb function ]
  == emb( plus (Succ n) m )       [ I.H of plus ]
  == emb( (Succ n) ) + emb( m )   [ def. emb function ]
  -X- use alternate def below if not satisfied at the step -X- 
  == emb( n ) + 1 + emb( m )      [ def. emb function ]
  == emb( n ) + emb( Succ m )     [ QED ]

-}

{-
  [
    Alternate solution for Inductive Step
    of 1) plus if above one does not satisfied.
  ]

  1) plus
  --------
  I.
  ----------Prove true for (n), (Succ m)

  emb( plus n (Succ m) )           [ def. emb function ]
  == emb( plus (Succ n) m )        [ I.H of plus ]
  == emb( plus (Succ (Succ n)) q ) [ m == Succ q ]
  == emb( plus (Succ (Succ n)) r ) [ m == Succ $ Succ r ]
  ...
  == emb( plus d Zero )
      [ m == Succ $ Succ $ Succ $ ... Zero ]
        { Succ appears m times }
      [ d == (Succ (Succ (Succ ( ... (Succ n) ... )))) ]
        { Succ appears m times on the left of (Succ n) }
  == emb ( d )
  == d
  == n + m + 1
  == emb( n ) + emb( Succ m ) [ QED ]

-}
{-
  2) times
  --------

  Base Case
  ---------

  emb( times Zero Zero ) 
  == emb( times _ Zero )
  == emb( Zero )
  == 0
  == 0 * 0
  == emb( Zero ) * emb( Zero )

  I.H
  ---------Assume holds for n, m

  emb( times n m )  == emb( n ) * emb( m )

  I.
  ----------Prove true for (n), (Succ m)

  emb( times n (Succ m) )   ==
  == emb( plus n $ times n m ) ==       [ def. 1) plus ]
  == emb ( n ) + ( emb( times n m ) )   [ I.H of times ]
  == emb ( n ) + ( emb (n) * emb (m) )  [ common ]
  == emb ( n ) * ( 1 + emb (m) )        [ def. emb function ]
  == emb ( n ) * emb (Succ m)           [ QED ]

-}
