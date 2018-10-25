-- 定义一个数组相加符号, 5表示该符号的优先级
infixr 5 :-: 
-- a :-: List 是一个构造函数, 所以可以被模式匹配
data List a = Empty | a :-: List a deriving(Show, Read, Eq, Ord)

infixr 6 .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

list = 3 :-: 4 :-: Empty

-- class Eq a where  
--  (==) :: a -> a -> Bool  
--  (/=) :: a -> a -> Bool  
--  x == y = not (x /= y)  
--  x /= y = not (x == y)  

-- 继承 Eq 并实现它定义的方法
-- 此处的 TrafficLight 就是a的实际类型
data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where  
  Red == Red = True  
  Green == Green = True  
  Yellow == Yellow = True  
  _ == _ = False

-- Maybe 是一个构造函数
-- Maybe m 才是具体类型
-- 声明 m 和 Maybe 都必须是 Eq 的子类
instance (Eq m) => Eq (Maybe m) where  
  Just x == Just y = x == y  
  Nothing == Nothing = True  
  _ == _ = False  
      
        