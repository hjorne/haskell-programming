import Data.List (intercalate)

replaceThe :: String -> String
replaceThe = intercalate " " . map (pullwords . notThe) . mywords 
    where pullwords Nothing = "a"
          pullwords (Just s) = s

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

mywords :: String -> [String]
mywords [] = []
mywords s = word : mywords (strip remainder)
    where isSpace c = c == ' '
          (word, remainder) = break isSpace s
          strip (' ':s) = s
          strip s = s

countTheBeforeVowel :: String -> Integer 
countTheBeforeVowel = countHelper . words
    where countHelper :: [String] -> Integer
          countHelper [_] = 0
          countHelper (s1:s2:xs) 
            | s1 == "the" && isVowel (head s2) = 1 + countHelper (s2 : xs)
            | otherwise                        =     countHelper (s2 : xs)


isVowel :: Char -> Bool
isVowel c = c `elem` ['a', 'e', 'i', 'o', 'u']

countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowel

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s | consonants > vowels = Just (Word' s)
         | otherwise           = Nothing
    where consonants = length . filter (not . isVowel) $ s
          vowels = length . filter isVowel $ s


data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat i 
    | i < 0 = Nothing
    | otherwise = case integerToNat (i - 1) of
        Nothing -> Nothing
        Just x -> Just $ Succ $ x