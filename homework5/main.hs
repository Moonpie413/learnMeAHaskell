module Calcualtor where
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = evalMaybe . parseExp Lit Add Mul

evalMaybe :: Maybe ExprT -> Maybe Integer
evalMaybe (Just x) = Just $ eval x
evalMaybe _ = Nothing