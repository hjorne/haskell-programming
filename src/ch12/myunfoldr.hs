myiterate :: (a -> a) -> a -> [a]
myiterate f z = z : myiterate f (f z)