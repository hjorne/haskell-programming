{-# LANGUAGE QuasiQuotes #-}

module AltParsing where

import Control.Applicative
import Text.RawString.QQ
import Text.Trifecta
import Data.Ratio ((%))

data RationalDecimal = 
      MyRational Rational 
    | MyDecimal Double
    deriving (Eq, Show)

type NumberOrString =
    Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
    skipMany newline >>
        (Left <$> integer) 
    <|> (Right <$> some letter)


mainalt = do
    let p f i =
            parseString f mempty i
    print $ p (some (token parseNos)) eitherOr

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|] 

