module QuickCheckBoolMonoid where

import QuickCheckMonoids

import Test.QuickCheck
import Data.Monoid
import Control.Monad

data Bull = Fools 
          | Twoo
          deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = elements [Fools, Twoo]

instance Semigroup Bull where
    (<>) Twoo _ = Twoo
    (<>) _ Twoo = Twoo
    (<>) _ _ = Fools

instance Monoid Bull where
    mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

testBull :: IO ()
testBull = do
    let ma = monoidAssoc
        mli = monoidLeftIdentity
        mri = monoidRightIdentity
    quickCheck (ma :: BullMappend)
    quickCheck (mli :: Bull -> Bool)
    quickCheck (mri :: Bull -> Bool)

