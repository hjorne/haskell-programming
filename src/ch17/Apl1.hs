module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--newtype ZipList a = ZipList [a] deriving (Eq, Show)
--
--instance Semigroup a => Semigroup (ZipList a) where
--  ZipList [] <> ZipList _  = ZipList []
--  ZipList _  <> ZipList [] = ZipList []
--  ZipList (x:xs) <> ZipList (y:ys) = ZipList $ x <> y : unzip zipXY
--    where zipXY = ZipList xs <> ZipList ys
--          unzip (ZipList x) = x
--
--instance Monoid a => Monoid (ZipList a) where
--  mempty = ZipList mempty

-- Yeah this is way better
instance Semigroup a => Semigroup (ZipList a) where
--  ZipList xs <> ZipList ys = ZipList $ zipWith (<>) xs ys
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (ZipList a) where
  mempty = ZipList []

-- Also already defined
--instance Arbitrary a => Arbitrary (ZipList a) where
--  arbitrary = ZipList <$> arbitrary

-- Already defined, apparently
--instance Arbitrary a => Arbitrary (Sum a) where
--  arbitrary = Sum <$> arbitrary


