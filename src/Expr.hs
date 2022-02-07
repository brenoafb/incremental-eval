module Expr where

data Expr = Add Expr Expr
          | Mult Expr Expr
          | Num Int
          deriving (Eq, Show)

eval :: Expr -> Int
eval (Num x) = x
eval (Add e e')  = eval e + eval e'
eval (Mult e e') = eval e * eval e'

op e = case e of
  (Add _ _)  -> (+)
  (Mult _ _) -> (*)
