type Height = Int

data Tree a = Leaf
            | Tree { height :: Height, left :: Tree a, elem :: a, right :: Tree a}
              deriving (Show, Eq)

-- 计算深度
calHeight :: Tree a -> Height
calHeight Leaf = 0
calHeight (Tree _ left _ right) = 1 + max (calHeight left) (calHeight right)

calMax :: Height -> Height -> Height
calMax a = (1+) . (a `max`)

foldTree :: [a] -> Tree a
foldTree _ = Leaf

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Leaf = Tree 1 Leaf x Leaf
insert x (Tree h left ele right)
  | x == ele = Tree h left x right
  | x > ele = Tree (calMax (calHeight left) (calHeight insertR)) left ele insertR
  | otherwise = Tree (calMax (calHeight insertL) (calHeight right)) insertR ele right
  where insertR = insert x right
        insertL = insert x left