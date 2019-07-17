lefts' :: [Either a b] -> [a]
lefts' = foldr f []
    where f (Left a) z = a : z
          f (Right _) z = z

rights' :: [Either a b] -> [b]
rights' = foldr f []
    where f (Right b) z = b : z
          f (Left _) z = z

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
    where f (Left a) (ls, rs) = (a:ls, rs)
          f (Right b) (ls, rs) = (ls, b:rs)

eitherMaybe :: (b -> c) -> Either a b -> Maybe c
eitherMaybe _ (Left _) = Nothing
eitherMaybe f (Right b) = Just $ f b

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b 

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (\_ -> Nothing) (\x -> Just $ f x) 