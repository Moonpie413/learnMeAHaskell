xor :: [Bool] -> Bool
xor =  (== 1) . (`mod` 2) . sumBool

sumBool :: [Bool] -> Int
sumBool = foldl (\acc x -> if x == True then acc + 1 else acc) 0
