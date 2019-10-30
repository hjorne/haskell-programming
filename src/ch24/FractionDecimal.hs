{-# LANGUAGE QuasiQuotes #-}

module FractionDecimal where

import Text.RawString.QQ
import Data.Ratio ((%))
import Text.Trifecta
import Control.Applicative

data FractionOrDecimal =
      Fraction Rational
    | Decimal Double
    deriving (Eq, Show)

parseRational :: Parser FractionOrDecimal
parseRational = 
    integer >>=
    \numerator -> char '/' >>
    integer >>= 
    \denominator -> pure $ Fraction $ numerator % denominator

parseDecimal :: Parser FractionOrDecimal
parseDecimal = 
    double >>=
    \d -> pure $ Decimal d

parseFractionOrDecimal :: Parser FractionOrDecimal
parseFractionOrDecimal = 
    try parseRational <|> try parseDecimal

parser :: Parser [FractionOrDecimal]
parser = 
    whiteSpace >> 
    (many $ token parseFractionOrDecimal)

base :: String
base = [r|
 12/23
334.21
1/2
39420432.432901
|]

mainDecimalFraction :: IO ()
mainDecimalFraction =
    print $ parseString parser mempty base