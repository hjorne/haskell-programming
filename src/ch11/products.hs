-- data Person = Person String Int deriving (Eq, Show)

data Person = 
    Person { name :: String
           , age :: Int }
           deriving (Eq, Show)
           