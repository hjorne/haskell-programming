-- Type constructer, data constructor
data Identity a = Identity' a

-- instance (Eq a, Eq b) => Eq (Identity a b) where
--     (==) (Identity x y) (Identity x' y') = x == x' && y == y'

instance Eq a => Eq (Identity a) where
    (==) (Identity' x) (Identity' x') = x == x'


