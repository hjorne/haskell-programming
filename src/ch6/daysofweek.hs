data DayOfWeek = Mon | Tues | Weds | Thurs | Fri | Sat | Sun deriving Show

data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
    (==) Mon Mon = True
    (==) Tues Tues = True
    (==) Weds Weds = True
    (==) Thurs Thurs = True
    (==) Fri Fri = True
    (==) Sat Sat = True
    (==) Sun Sun = True
    (==) _ _ = False 

instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _ = GT
    compare _ Fri = LT
    compare _ _ = EQ 

instance Eq Date where
    (==) (Date weekday dayOfMonth)
         (Date weekday' dayOfMonth') =
        weekday == weekday' && dayOfMonth == dayOfMonth'

f :: Int -> String
f 2 = "hi"
f 1 = "hi"
f 3 = "hi"
f _ = "hi"