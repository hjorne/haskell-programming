data TisAnInteger = TisAn Integer
data TwoIntegers = Two Integer Integer
data StringOrInt = TisAnInt Int | TisAString String
data Pair a = Pair a a
data Tuple a b = Tuple a b
data Which a = ThisOne a | ThatOne a
data EitherOr a b = Hello a | Goodbye b

data Pointless = Pointless

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn x') = x == x'

instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') = x == x' && y == y'

instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt x') = x == x'
    (==) (TisAString s) (TisAString s') = s == s'

instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye a) (Goodbye a') = a == a'