module IdentityApplicative (
    Identity, 
    Constant) 
where 

newtype Identity a = 
    Identity a 
    deriving (Eq, Show)

xs :: [Int]
xs = [1, 2, 3]

xs' :: [Int]
xs' = [9, 9, 9]

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    Identity f <*> Identity a = Identity $ f a
    pure = Identity 

instance Semigroup a => Semigroup (Constant a b) where
    (Constant a) <> (Constant b) = Constant (a <> b)

instance Monoid a => Monoid (Constant a b) where
    mempty = Constant mempty

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

-- The "e" is part of the structor that gets lifted over
instance Functor (Constant e) where
    fmap _ (Constant a) = Constant a

instance Monoid e => Applicative (Constant e) where
    pure _ = Constant mempty
    Constant a <*> Constant b = Constant (a <> b)