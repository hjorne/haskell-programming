isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _ Nothing = z
mayybee _ f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe z = mayybee z id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f []
    where f Nothing z = z 
          f (Just x) z = x : z

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
    where f Nothing _ = Nothing
          f _ Nothing = Nothing
          f (Just x) (Just xs) = Just (x:xs)