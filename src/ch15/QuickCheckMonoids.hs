module QuickCheckMonoids where

import Test.QuickCheck
import Data.Monoid

monoidAssoc :: (Eq m, Monoid m) 
            => m -> m -> m -> Bool
monoidAssoc a b c =
    a <> (b <> c) == (a <> b) <> c

testMonoid :: IO ()
testMonoid = quickCheck (monoidAssoc :: String -> String -> String -> Bool)

monoidLeftIdentity :: (Eq m, Monoid m) 
                   => m -> Bool
monoidLeftIdentity m = mempty <> m == m 

monoidRightIdentity :: (Eq m, Monoid m)
                    => m -> Bool
monoidRightIdentity m = m <> mempty == m

-- monoidLeftAndRightIdentities :: (Eq m, Monoid m)
--                              => m -> Bool
-- monoidLeftAndRightIdentities m = (m <> mempty == m) && (mempty <> m == m)