newtype DaPhone = DaPhone [(Digit, String)]
                deriving (Eq, Show, Ord)

type Digit = Char
type Presses = Int

phone :: DaPhone
phone = DaPhone
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

convo :: [String] 
convo =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)] 
reverseTaps (DaPhone keypad) c = undefined
    where index = map (\(digit, letters) -> ) 
-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)] 
cellPhonesDead = undefined