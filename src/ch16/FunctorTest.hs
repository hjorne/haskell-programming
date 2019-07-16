module FunctorTest where

import Test.QuickCheck

-- instance Functor (Two a) where
--     fmap f (Two a b) = Two $ (f a) (f b)

-- instance Functor (Two a) where
--     fmap f (Two a b) = Two a (f b)

functorIdentity :: (Functor f, Eq (f a)) 
                => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c))
               => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = 
    (fmap (g . f) x) == ((fmap g) . (fmap f) $ x)

-- fcmp :: (Functor f, Eq (f c))
--      => f a -> Bool
-- fcmp = functorCompose



-- f :: [Int] -> Bool
-- f x = functorIdentity x

-- g :: [Int] -> Bool
-- g x = functorCompose (+1) (*2) x

-- fmain = do
--     quickCheck f
--     quickCheck g

newtype Identity a = Identity a deriving (Eq, Show)
data Pair a = Pair a a deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Three' a b = Three' a b b deriving (Eq, Show)

fmain = do
    quickCheck (functorIdentity :: Identity Int -> Bool)
    quickCheck (functorCompose (+1) (*2) :: Identity Int -> Bool)

    quickCheck (functorIdentity :: Pair Int -> Bool)
    quickCheck (functorCompose (+1) (*2) :: Pair Int -> Bool)

    quickCheck (functorIdentity :: Two Int Int -> Bool)
    quickCheck (functorCompose (+1) (*2) :: Two Int Int -> Bool)

    quickCheck (functorIdentity :: Three Int Int Int -> Bool)
    quickCheck (functorCompose (+1) (*2) :: Three Int Int Int -> Bool)

    quickCheck (functorIdentity :: Three' Int Int -> Bool)
    quickCheck (functorCompose (+1) (*2) :: Three' Int Int -> Bool)


instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Pair a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three' a b c
