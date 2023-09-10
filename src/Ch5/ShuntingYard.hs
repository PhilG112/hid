module Ch5.ShuntingYard where

data Expr a = Lit a | Add (Expr a) (Expr a) | Mult (Expr a) (Expr a)

type Token = String
type Stack = [Token]
type Output = [Expr Integer]
type SYState = (Stack, Output)