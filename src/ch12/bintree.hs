data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f z = case f z of
    Just (a1, b, a2) -> Node (unfold f a1) b (unfold f a2)
    Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
    where f n' | n' < n    = Just (n' + 1, n', n' + 1)
               | otherwise = Nothing