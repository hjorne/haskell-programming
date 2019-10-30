{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ini where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
import Text.Trifecta

data Section = 
    Section Header Assignments 
    deriving (Eq, Show)

newtype Header = 
    Header String 
    deriving (Eq, Show, Ord)

newtype Config = 
    Config (Map Header Assignments)
    deriving (Eq, Show)

type Name = String
type Value = String
type Assignments = Map Name Value

headerEx :: ByteString
headerEx = "[blah]"

assignmentEx :: ByteString
assignmentEx = "woot=1 testing\n\n\nthis=whatup"

commentEx :: ByteString
commentEx = "; last modified 1 april 2001 john doe"

commentEx' :: ByteString
commentEx' = ";this is\n;a test"

sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString 
sectionEx' = [r|
; ignore me
[states]
Chris=Texas 
|]

sectionEx'' :: ByteString 
sectionEx'' = [r|
; comment
[section] 
host=wikipedia.org 
alias=claw

[whatisit] 
red=intoothandclaw 
|]


parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
    char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader =
    parseBracketPair (Header <$> some letter)

parseAssignment :: Parser (Name, Value)
parseAssignment = 
    some letter >>=
    \name -> char '=' >>
    some (noneOf "\n") >>= 
    \value -> skipEOL >>
    pure (name, value) 

parseAssignments :: Parser [(Name, Value)]
parseAssignments = some parseAssignment

skipComments :: Parser ()
skipComments = skipMany (
        char ';' <|> char '#' >>
        skipMany (noneOf "\n") >>
        skipEOL
    )

-- Skip end of line whitespace and beyond.
-- Ensures parser is on next token when needed
skipEOL :: Parser ()
skipEOL = skipMany (oneOf " \n")

skipWhitespace :: Parser ()
skipWhitespace =
    skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
    skipWhitespace
    skipComments
    h <- parseHeader
    skipEOL
    assignments <- some parseAssignment
    return $ Section h (M.fromList assignments)

rollup :: Section 
       -> Map Header Assignments 
       -> Map Header Assignments
rollup (Section h a) m =
    M.insert h a m

parseIni :: Parser Config
parseIni = do
    sections <- some parseSection
    let mapOfSelections = 
            foldr rollup M.empty sections
    pure $ Config mapOfSelections

mainini :: IO ()
mainini = do
    print $ parseByteString parseHeader mempty headerEx
    print $ parseByteString parseAssignments mempty assignmentEx
    print $ parseByteString parseIni mempty sectionEx''