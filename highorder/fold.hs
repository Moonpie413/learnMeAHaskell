foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ r [] = r
foldl' f r (x:xs) = foldl' f (f r x) xs

foldr' :: (b -> a -> a) -> a -> [b] -> a
foldr' f r = foldl' (flip f) r . reverse

map' :: (a -> b) -> [a] -> [b]
map' f = foldr' (\x acc -> f x : acc) []

-- foldl' _ r [] = r
-- foldl' f r (x:xs) = f f r x 

reverse' :: [a] -> [a]  
reverse' = foldl' (\acc x -> x : acc) [] 