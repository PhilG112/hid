{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module CH7.EvalRPNExcept where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Text hiding (length)
import Data.Text.Internal.Builder (fromText)
import TextShow (Builder, TextShow (showb))
import Data.Text.Read
import Data.Foldable

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

readVar :: Text -> EvalM Integer
readVar name = do
    var <- asks (\s -> lookup name s)
    case var of
        Just n -> pure n
        Nothing -> throwError $ UnkownVar name

readNumber :: Text -> EvalM Integer
readNumber txt =
    case decimal txt of
        Right (n, rest) | Data.Text.null rest -> pure n
        _ -> throwError $ NotANumber txt

evalRPNOnce :: Text -> EvalM Integer
evalRPNOnce str =
    clearStack >> traverse_ step (Data.Text.words str) >> oneElementOnStack >> pop
    where
        clearStack = put []
        step "+" = processTops (+)
        step "-" = processTops (-)
        step "*" = processTops (*)
        step t = read t >>= push
        processTops op = op <$> pop <*> pop >>= push
        