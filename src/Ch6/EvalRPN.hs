module Ch6.EvalRPN where
import Control.Monad.State
import Control.Monad.Identity

type Stack = [Integer]
type EvalM = State Stack

push :: Integer -> EvalM ()
push i = modify (\s -> i:s)

isEmpty :: EvalM Bool
isEmpty = null <$> get

notEmpty :: EvalM Bool
notEmpty = not <$> isEmpty

oneElementOnStack :: EvalM Bool
oneElementOnStack = do
    len <- length <$> get
    pure (len == 1)

pop :: EvalM Integer
pop = do
    s <- get
    put (tail s)
    pure (head s)

evalRPN :: String -> Integer
evalRPN expr = evalState evalRPN' []
    where
        evalRPN' = traverse step (words expr) >> pop

        step "+" = processTops (+)
        step "*" = processTops (*)
        step "-" = processTops (-)
        step t = push (read t)

        processTops op = flip op <$> pop <*> pop >>= push