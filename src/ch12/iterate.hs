myiterate :: (a -> a) -> a -> [a]
myiterate f z = z : myiterate f (f z)

myunfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myunfoldr f z = case f z of 
    Just (a, b) -> a : myunfoldr f b
    Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f z = myunfoldr f' z
    where f' x = Just (x, f x)

