{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return $ numerator % denominator

mainfrac :: IO ()
mainfrac = do
    let parseFraction' = parseString parseFraction mempty
    print $ parseFraction' alsoBad
    print $ parseFraction' badFraction
    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork

inteof :: Parser Integer
inteof = integer >>= 
    \i -> eof >>
    pure i

inteof' :: Parser Integer
inteof' = do
    i <- integer
    eof
    pure i