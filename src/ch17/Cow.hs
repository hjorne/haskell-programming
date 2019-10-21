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

cowBuilder3 :: String -> Int -> Int -> Maybe Cow
cowBuilder3 name' age' weight' =
    liftA3 Cow (noEmpty name')
               (noNegative age')
               (noNegative weight')