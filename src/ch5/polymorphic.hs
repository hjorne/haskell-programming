module Polymorphic where

id' :: a -> a
id' x = id' x

idFunc :: a -> a -> a
idFunc x y = x

idFunc2 :: a -> a -> a
idFunc2 x y = y

idFunc3 :: a -> b -> b
idFunc3 x y = y