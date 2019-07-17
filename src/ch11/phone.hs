import qualified Data.Map as Map

keypad :: [(Char, String)]
keypad = 
    [
        ('1' , " "),
        ('2', "abc"),
        ('3', "def"),
        ('4', "ghi"),
        ('5', "jkl"),
        ('6', "mno"),
        ('7', "pqrs"),
        ('8', "tuv"),
        ('9', "wxyz"),
        ('*', "^"),
        ('0', "+_"),
        ('#', ".,")
    ]

toCaps :: Char -> Char
toCaps = (Map.!) capsMap
    where capsMap = Map.fromList $ zip ['a'..'z'] ['A'..'Z']

expandTuple :: (Char, String) -> [(Char, [(Char, Int)])]
expandTuple (c, str) = map (\(a, b) -> (a, [b])) $ flipTuple z2
    where z1 = zip (str ++ [c]) [1..] 
          z2 = zip (repeat c) z1 

flipTuple :: [(a, (b, c))] -> [(b, (a, c))]
flipTuple = map f
    where f (a, (b, c)) = (b, (a, c))

flatten :: [[a]] -> [a]
flatten = foldr (++) [] 

keypadLower :: [(Char,[(Char, Int)])]
keypadLower = flatten . map expandTuple $ keypad

keymap :: Map.Map Char [(Char, Int)]
keymap = Map.fromList $ keypadCaps ++ keypadLower
    where keypadCaps = map f . filter isAlpha $ keypadLower
          f (c, lst) = (toCaps c, ('*', 1) : lst)
          isAlpha = flip elem ['a'..'z'] . fst

msgToKeys :: String -> String
msgToKeys = flatten . map handleChar
    where handleChar c = flattenKeys $ keymap Map.! c
          flattenKeys = flatten . map (\(char, times) -> replicate times char)