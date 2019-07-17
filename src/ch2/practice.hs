module Mult1 where

mult1       = x * y
    where x = 5
          y = 6

prac1let    = let x = 3; y = 1000 in x * 3 + y
prac1where  = x * 3 + y
    where x = 3
          y = 1000

prac2let    = let y = 10; x = 10 * 5 + y in x * 5
prac2where  = x * 5
    where x = 10 * 5 + y
          y = 10

prac3let   = let x = 7
                 y = negate x 
                 z = y * 10
             in z / x + y
prac3where = z / x + y
    where x = 7
          y = negate x
          z = y * 10

checkOut = do print prac1let
              print prac1where
              print prac2let
              print prac2where
              print prac3let
              print prac3where

waxOn       = x * 5
    where x = y ^ 2
          y = z + 8
          z = 7

triple x = x * 3

waxOff x = triple x