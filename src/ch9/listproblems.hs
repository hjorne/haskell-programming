module ListProblems where

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f []     = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) = x == y || myElem y xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' y = myAny (\x -> x == y)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (xs:xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMax :: Ord a => a -> a -> a
myMax x y 
   | x > y     = x
   | otherwise = y

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy cmp (x:xs) = 
    case cmp x y of 
        GT -> x
        _  -> y
    where y = myMaximumBy cmp xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy cmp (x:xs) = 
    case cmp x y of 
        LT -> x
        _  -> y
    where y = myMinimumBy cmp xs

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare 

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare

