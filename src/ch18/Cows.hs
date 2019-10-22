module Cows where

data Cow = Cow {
      name   :: String
    , age    :: Integer
    , weight :: Integer
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s  = Just s

noNegative :: Integer -> Maybe Integer
noNegative x | x >= 0    = Just x
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c = 
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
       then Nothing
       else Just c

cowBuilder :: String -> Integer -> Integer -> Maybe Cow
cowBuilder name' age' weight' =
    case noEmpty name' of
        Nothing -> Nothing
        Just n  ->
            case noNegative weight' of
                Nothing -> Nothing
                Just w  ->
                    case noNegative age' of
                        Nothing -> Nothing
                        Just a ->
                            weightCheck $ Cow n a w

cowBuilder' :: String -> Integer -> Integer -> Maybe Cow
cowBuilder' name' age' weight' = do
    n <- noEmpty name'
    a <- noNegative age'
    w <- noNegative weight'
    weightCheck $ Cow n a w

cowBuilder'' :: String -> Integer -> Integer -> Maybe Cow
cowBuilder'' name' age' weight' = 
    noEmpty name' >>=
        \n -> noNegative age' >>=
            \a -> noNegative weight' >>=
                \w -> weightCheck $ Cow n a w