doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

length' xs = sum [1 | _ <- xs]

length'' :: [a] -> Int
length'' [] = 0
length'' (x:xs) = 1 + length'' xs

removeNonUppercase xs = [x | x <- xs, x `elem` ['A'..'Z']]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a
head' [] = error "no head"
head' (x:_) = x

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "to thin"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "fat"
  | otherwise = "dont know"
  where bmi = weight / height ^ 2

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum at an empty string"
maximum' [x] = x
maximum' (x:xs)
  | x > maxtail = x
  | otherwise = maxtail
  where maxtail = maximum' xs

replicate' :: (Num i, Ord i) => i -> i -> [i]
replicate' x y
  | x <= 0 = []
  | otherwise = y:replicate' (x - 1) y

take' :: (Num i, Ord i) => i -> [i] -> [i]
-- take' n all@(x:xs)
  -- | n <= 0 = []
  -- | all == [] = []
  -- | otherwise = x:take' (n - 1) xs
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n - 1) xs

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort[a | a <- xs, a <= x]
  ++ [x] ++ quickSort[a | a <- xs, a > x]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x = x:filter' f xs
  | otherwise = filter' f xs


quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
  let smaller = filter' (<=x) xs
      bigger = filter' (>x) xs
  in quickSort' smaller ++ [x] ++ quickSort' bigger

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ x [] = x
foldl' f acc (x:xs) = 
  let next = f acc x
  in foldl' f next xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concatMap' :: (a -> b) -> [[a]] -> [b]
concatMap' _ [] = []
concatMap' f (x:xs) = map f x ++ concatMap' f xs

sumr = sum . replicate 5 . max 6

-- sum' = foldl (+) 0

oddSquareSum :: (Integral a) => [a] -> a
-- oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))     
oddSquareSum =  sum . takeWhile (<1000) . filter odd . map (^2)

phoneBook = [("book", "1"), ("toy", "2")]
findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key = snd .head . filter ((key==) . snd)