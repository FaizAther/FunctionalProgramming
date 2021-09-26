module Prop_Test (checkPs) where

import Prop

-- (A and B) => A (tautology)
p2 :: Prop
p2 = Or (Not (And (Var "A") (Var "B"))) (Var "A")

-- (A and (A => B)) => B (tautology)
p4 :: Prop
p4 = Or (Not (And (Var "A") (Or (Not (Var "A")) (Var "B")))) (Var "B")

-- (A or not A) (tautology)
p5 :: Prop
p5 = Or (Var "p1") (Not (Var "p1"))

-- (A => B) <=> (not B => not A) (tautology)
p6 :: Prop
p6 =
  And
    (Or (Not (Or (Not (Var "A")) (Var "B"))) (Or (Var "B") (Not (Var "A"))))
    (Or (Not (Or (Var "B") (Not (Var "A")))) (Or (Not (Var "A")) (Var "B")))

-- ((A => B) and (B => C)) => (A => C) (tautology)
p7 :: Prop
p7 = Or (Not (And (Or (Not (Var "A")) (Var "B")) (Or (Not (Var "B")) (Var "C")))) (Or (Not (Var "A")) (Var "C"))

pT :: [Prop]
pT = [p2, p4, p6, p5, p7]

-- True

px :: Prop
px = And (Var "p") (Var "q")

py :: Prop
py = Or px (Var "r")

pz :: Prop
pz = Not py

pa :: Prop
pa = And pz (Var "s")

pb :: Prop
pb = Or pa (Var "p")

pF :: [Prop]
pF = [px,py,pz,pa,pb]

-- False

truePs :: Bool
truePs = foldr ((&&) . tautology) True pT

falsePs :: Bool
falsePs = foldr ((&&) . not . tautology) True pF

checkPs :: Bool
checkPs = truePs && falsePs