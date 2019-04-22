module FirstMonoid where

import QuickCheckMonoids 

import Test.QuickCheck
import Data.Monoid

data Optional a = Nada
                | Yep a
                deriving (Eq, Show)

newtype First' a =
    First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        oneof [return $ First' Nada,
               return $ First' (Yep a)]

instance Semigroup (First' a) where
    First' (Yep a) <> First' (Yep b) = First' (Yep a)
    First' Nada <> First' (Yep b) = First' (Yep b)
    First' (Yep a) <> First' Nada = First' (Yep a)
    First' Nada <> First' Nada = First' Nada

instance Monoid (First' a) where 
    mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
       First' String
    -> First' String
    -> First' String
    -> Bool
    
type FstId = First' String -> Bool

testFirstMonoid :: IO () 
testFirstMonoid = do
    quickCheck (monoidAssoc :: FirstMappend) 
    quickCheck (monoidLeftIdentity :: FstId) 
    quickCheck (monoidRightIdentity :: FstId)