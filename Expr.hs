-- | Library using binary trees to represent expressions

module Expr where

data Expr = Const Float | Add Expr Expr | Sub Expr Expr | Mult Expr Expr | Div Expr Expr

evaluate :: Expr -> Float
evaluate (Const x) = x
evaluate (Add ex ey) = evaluate ex + evaluate ey
evaluate (Sub ex ey) = evaluate ex - evaluate ey
evaluate (Mult ex ey) = evaluate ex * evaluate ey
evaluate (Div ex ey) = evaluate ex / evaluate ey
