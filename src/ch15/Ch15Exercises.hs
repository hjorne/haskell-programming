module Ch15Exercises where

import QuickCheckMonoids
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show) 
newtype Identity a = Identity a deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
newtype BoolConj = BoolConj Bool deriving (Eq, Show)
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity b = Identity (a <> b)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a1 b1 <> Two a2 b2 = Two (a1 <> a2) (b1 <> b2)

instance Semigroup BoolConj where
    BoolConj a <> BoolConj b = BoolConj (a && b)

instance Semigroup BoolDisj where
    BoolDisj a <> BoolDisj b = BoolDisj (a || b)

instance Semigroup (Or a b) where
    Snd a <> _ = Snd a
    _ <> Snd a = Snd a
    _ <> a = a

instance Arbitrary Trivial where
    arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return (Identity a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance Arbitrary BoolConj where
    arbitrary = elements [BoolConj True, BoolConj False]

instance Arbitrary BoolDisj where
    arbitrary = elements [BoolDisj True, BoolDisj False]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary 
        b <- arbitrary 
        oneof [return $ Fst a, 
               return $ Fst b]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool
type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

runCh15 :: IO () 
runCh15 = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)
    quickCheck (semigroupAssoc :: CombineAssoc)


newtype Combine a b = Combine { unCombine :: (a -> b) } 

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
    f <> g = Combine h'
        where f' = unCombine f
              g' = unCombine g
              h' x = f' x <> g' x

type CombineAssoc = Combine String String -> Combine String String -> Combine String String -> Bool