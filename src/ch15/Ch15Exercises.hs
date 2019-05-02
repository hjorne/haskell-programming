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

instance Monoid Trivial where
    mempty = Trivial

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity b = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a1 b1 <> Two a2 b2 = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

instance Semigroup BoolConj where
    BoolConj a <> BoolConj b = BoolConj (a && b)

instance Monoid BoolConj where
    mempty = BoolConj True

instance Semigroup BoolDisj where
    BoolDisj a <> BoolDisj b = BoolDisj (a || b)

instance Monoid BoolDisj where
    mempty = BoolDisj False

instance Semigroup (Or a b) where
    Snd a <> _ = Snd a
    _ <> Snd a = Snd a
    _ <> Fst a = Fst a

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

monoidLeftAndRightIdentities :: (Eq m, Monoid m) => m -> Bool
monoidLeftAndRightIdentities m = (m <> mempty == m) && (mempty <> m == m)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool
type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

type TrivIdentity = Trivial -> Bool
-- >_>
type IdentityIdentity = Identity String -> Bool
type TwoIdentity = Two String String -> Bool
type OrIdentity = Or String String -> Bool

runCh15 :: IO () 
runCh15 = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdentityAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)
    quickCheck (monoidLeftAndRightIdentities :: TrivIdentity)
    quickCheck (monoidLeftAndRightIdentities :: IdentityIdentity)
    quickCheck (monoidLeftAndRightIdentities :: TwoIdentity)


-- Don't know how to come up with an Arbitrary instance for this type
newtype Combine a b = Combine { unCombine :: (a -> b) } 

instance (Semigroup a, Semigroup b) => Semigroup (Combine a b) where
    f <> g = Combine h'
        where f' = unCombine f
              g' = unCombine g
              h' x = f' x <> g' x

type CombineAssoc = Combine String String -> Combine String String -> Combine String String -> Bool

newtype Comp a = Comp (a -> a)

instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp (f . g)

newtype Mem s a = 
    Mem {
        runMem :: s -> (a, s)
    }

instance Semigroup a => Semigroup (Mem s a) where
    Mem f <> Mem g = Mem $ h
        where h s = (af <> ag, sg)
                where (af, sf) = f s
                      (ag, sg) = g sf

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)

f' = Mem $ \s -> ("hi", s + 1)

ch15RunMem = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0 
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int)) 
    print $ rmleft == runMem f' 0 
    print $ rmright == runMem f' 0
