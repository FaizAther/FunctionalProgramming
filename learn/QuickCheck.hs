
import Test.QuickCheck

prop0 a b = (a+b) == (b+a)

--  *** Failed! Exception: 'Prelude.tail: empty list' (after 1 test):
--  []
prop1 xs = (length $ tail xs) == ((length xs)-1)

prop2 xs = 
  not (null xs) ==>
  (length $ tail xs) === ((length xs)-1)
