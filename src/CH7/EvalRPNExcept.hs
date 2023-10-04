{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module CH7.EvalRPNExcept where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Text hiding (length)
import Data.Text.Internal.Builder (fromText)
import TextShow (Builder, TextShow (showb))

type EvalM a = ReaderT EnvVars (ExceptT EvalError (State Stack)) a

type Stack = [Integer]

type EnvVars = [(Text, Integer)]

data EvalError
    = NotEnoughElements
    | ExtraElements
    | NotANumber Text
    | UnkownVar Text

instance TextShow EvalError where
    showb :: EvalError -> Builder
    showb NotEnoughElements = "Not enough elements in the expression"
    showb ExtraElements = "There are extra elements in the expression"
    showb (NotANumber t) =
        "Expression component '"
            <> fromText t
            <> "' is not a number"
    showb (UnkownVar t) =
        "Variable '"
            <> fromText t
            <> "' notfound"

push :: Integer -> EvalM ()
push x = modify (\stack -> x : stack)

pop :: EvalM Integer
pop = get >>= pop'
    where
        pop' :: Stack -> EvalM Integer
        pop' [] = throwError NotEnoughElements
        pop' (x:xs) = put xs >> pure x

oneElementOnStack :: EvalM()
oneElementOnStack = do
    len <- gets (\stack -> length stack)
    when (len /= 1) $ throwError ExtraElements