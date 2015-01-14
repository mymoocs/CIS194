module ExprT where

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String
              deriving (Show, Eq)
