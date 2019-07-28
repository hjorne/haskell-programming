module BadMonoid where

import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = oneof [pure Fools, pure Twoo]

instance Semigroup Bull where
  (<>) _ _ = Twoo

instance Monoid Bull where
  mempty = Fools

instance EqProp Bull where
  (=-=) = eq

test :: IO ()
test = quickBatch (monoid Twoo)

trigger :: [(String, String, Int)]
trigger = undefined

testApplicative :: IO ()
testApplicative = quickBatch $ applicative trigger

