module BoopDoop where

import Control.Applicative
import Data.Char

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)

bip :: Num a => a -> a
bip = boop . doop

-- What structure is "boop" being "lifted"
-- over here? (->) r? It's being lifted over the
-- function argument. Surprisingly intuitive

-- If you could make all kinds of stuff lifted over function
-- args, then of course you could have access to those args
-- which is basically what is wanted/required by DI

-- Here, it's a partially applied function
bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

cap :: String -> String
cap = fmap toUpper 

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = cap <$> rev

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

tupledMonad :: String -> (String, String)
tupledMonad = do
    s1 <- rev
    s2 <- cap
    return (s1, s2)

tupledMonad' :: String -> (String, String)
tupledMonad' = cap >>= \s1 -> rev >>= \s2 -> return (s1, s2)
