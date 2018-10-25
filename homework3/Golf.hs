module Golf where

-- 从数组中每隔 n 个元素筛选出来
-- 思路为首先将数组中能整除n的索引筛出来, 再根据这些索引去取值
pick :: Int -> [a] -> [a]
pick _ [] = []
pick n list
  | n <= 0 = []
  | otherwise = [list !! (x - 1) | x <- [1.. length list], x `mod` n == 0]

-- 思路为首先获取list的索引值
-- 然后将每个索引值用来从原数组中取该索引值应该对应的数组
skips :: [a] -> [[a]]
skips [] = []
skips lst = map (`pick` lst) [1..length lst]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > x && y > z = y : localMaxima (y:z:xs)
  | otherwise = localMaxima (y:z:xs)
localMaxima _ = []

histogram :: [Int] -> String
histogram lst =  (unlines $ map (genLine' lst) [9, 8 ..1])
  ++ "==========\n0123456789\n"

-- 生成 * 以及 空格组成的行
genLine :: [Int] -> Int -> String
genLine lst lineNumber = map (\x -> if (lineNumber > x) then ' ' else '*') lst

-- 用列表生成式实现, 并集成countLine
genLine' :: [Int] -> Int -> String
genLine' xs num = [if num > x then ' ' else '*' | x <- countLine xs]

countLine :: [Int] -> [Int]
countLine lst = map (`count'` lst) [0..9]

-- 统计某一个元素在数组中存在的次数
count :: (Eq a) => a -> [a] -> Int
count _ [] = 0
count i (x:xs)
  | i == x = 1 + next
  | otherwise = next
  where next = count i xs

count' :: (Eq a) => a -> [a] -> Int
count' _ [] = 0
count' i lst = length $ filter (==i) lst
