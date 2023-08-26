{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch2.Expr where

import TextShow

data Expr a
    = Lit a
    | Add (Expr a) (Expr a)
    | Mult (Expr a) (Expr a)

myEval :: Num a => Expr a -> a
myEval (Lit e) = e
myEval (Add e1 e2) = myEval e1 + myEval e2
myEval (Mult e1 e2) = myEval e1 * myEval e2

instance TextShow a => TextShow (Expr a) where
    showbPrec :: TextShow a => Int -> Expr a -> Builder
    showbPrec p e =
        case e of
            Lit a -> showb a
            Add e1 e2 -> showbHelper p 5 "+" e1 e2
            Mult e1 e2 -> showbHelper p 6 "*" e1 e2
        where
            showbHelper outerPrec thisPrec op e1 e2 =
                showbParen (outerPrec > thisPrec) $
                    showbPrec thisPrec e1 <> op <> showbPrec thisPrec e2