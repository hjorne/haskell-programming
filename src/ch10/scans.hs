module Scans where

foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' _  acc []    = acc
foldl'' f acc (x:xs) = foldl'' f (f acc x) xs

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' _ acc [] = [acc]
scanl' f acc (x:xs) = acc : scanl' f (f acc x) xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' _ z [] = [z]
scanr' f z (x:xs) = f x (head ls) : ls
    where ls = scanr' f z xs