{-# LANGUAGE OverloadedStrings #-}

module LearnParsers where

import Text.Trifecta
import Text.Parser.Combinators (eof)

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
    print $ parseString p mempty "123"

pNL :: String -> IO ()
pNL s =
    putStrLn ('\n':s)

parsemain = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':" 
    testParse one' 
    pNL "oneTwo:" 
    testParse oneTwo 
    pNL "oneTwo':" 
    testParse oneTwo'

oneEof :: Parser ()
oneEof = char '1' >> eof

p123 :: String -> IO ()
p123 s = 
    print $ parseString parser mempty s
    where parser = choice [string "123", string "12", string "1"]
