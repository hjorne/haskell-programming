module DB where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem] 
theDatabase =
    [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123)) , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
    where f (DbDate x) acc = x : acc
          f _ acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
    where f (DbNumber x) acc = x : acc
          f _ acc = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral $ sumDb db) / (fromIntegral $ length $ filterDbNumber db)