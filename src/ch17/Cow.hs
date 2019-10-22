module Cow where

import Control.Applicative (liftA3)

data Cow = Cow {
      name   :: String
    , age    :: Int
    , weight :: Int
    } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s  = Just s

noNegative :: Int -> Maybe Int
noNegative x | x < 0     = Nothing
             | otherwise = Just x

cowBuilder :: String -> Int -> Int -> Maybe Cow
cowBuilder name' age' weight' = 
    case noEmpty name' of
        Nothing          -> Nothing
        Just noEmptyName -> 
            case noNegative age' of
                Nothing            -> Nothing
                Just noNegativeAge -> 
                    case noNegative weight' of
                        Nothing               -> Nothing
                        Just noNegativeWeight -> 
                            Just $ Cow noEmptyName noNegativeAge noNegativeWeight

cowBuilder2 :: String -> Int -> Int -> Maybe Cow
cowBuilder2 name' age' weight' =
    Cow <$> noEmpty name' 
        <*> noNegative age' 
        <*> noNegative weight'

weightCheck :: Cow -> Maybe Cow
weightCheck c = 
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
       then Nothing
       else Just c

-- This is clearly terrible
monadicCow :: String -> Int -> Int -> Maybe Cow
monadicCow name' age' weight' = 
    let c = Cow <$> noEmpty name' 
                <*> noNegative age' 
                <*> noNegative weight'
    in case c of
        Nothing -> Nothing
        Just cow -> weightCheck cow


cowBuilder3 :: String -> Int -> Int -> Maybe Cow
cowBuilder3 name' age' weight' =
    liftA3 Cow (noEmpty name')
               (noNegative age')
               (noNegative weight')