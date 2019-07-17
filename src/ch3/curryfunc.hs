module CurryFunc where

p1 s = take 16 s

p2 s = s !! 4

p3 s = drop 9 s

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! (x + 1)

rvrs s = concat [third, " ", second, " ", first]
    where third = drop 9 s
          second = take 2 (drop 6 s)
          first = take 5 s
