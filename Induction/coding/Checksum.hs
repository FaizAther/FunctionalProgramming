module Checksum (checkSum) where
import Data.Char
{--
A valid credit card number satisfies a property known as a CHECK SUM.

The check sum for credit cards is as follows.  If n :: Integer satisfies
1/  n is a 16 digit number XXXX XXXX XXXX XXXX (the blanks aren't really there).

Designating odd "O" and even "E" positions in n by
  OEOE OEOE OEOE OEOE
then
2/  If the (sum of the odd digits) + 2*(sum of even digits) is divisilbe by 10
    then the card is valued.

For example,
OEOE OEOE OEOE OEOE
1234 5678 9012 3452
(1+3+5+7+9+1+3+5) + 2*(2+4+6+8+0+2+4+2)
= 90
which is divisible by 10 and therefore valid.

1111 1111 1111 1111
8 + 2*8
= 24
which is NOT divisible by 10 and therefore NOT valid.

=======
EXAMPLE
=======
> checkSum 1234567890123452
True
> checkSum 1111111111111111
False
--}

toInt :: Char -> Int
toInt c = ord c - 48

splitOE :: String -> (Int, Int)
splitOE [] = (0, 0)
splitOE [x] = (toInt x + 0, 0)
splitOE [x, y] = (toInt x + 0, toInt y + 0)
splitOE (x:y:ys) = let (o, e) = splitOE ys
                    in (toInt x + o, toInt y + e)

checkSum :: Integer -> Bool
checkSum i = let (o,e) = splitOE $ show i
              in rem (o + 2*e) 10 == 0
