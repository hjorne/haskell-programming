module Folds where

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' _ acc [] = acc
foldl'' f acc (x:xs) = foldl'' f (f acc x) xs

foldrTest :: String
foldrTest = foldr' f "0" (map show [1..4])
    where f x y = concat ["(", x, "+", y, ")"]

foldlTest :: String
foldlTest = foldl'' f "0" (map show [1..4])
    where f x y = concat ["(", x, "+", y, ")"]
