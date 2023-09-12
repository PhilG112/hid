module Ch6.EvalRPNTrans where

import Control.Applicative
import Control.Monad.State
    ( MonadState (get, put),
      MonadTrans (lift),
      StateT,
      guard,
      modify',
      when, evalStateT,
    )
import Text.Read (readMaybe)

type Stack = [Integer]

type EvalM = StateT Stack Maybe

push :: Integer -> EvalM ()
push i = modify' (i :)

pop :: EvalM Integer
pop = do
    all@(x : xs) <- get
    guard (not $ null all)
    put xs
    pure x

oneElementOnStack :: EvalM ()
oneElementOnStack = do
    l <- length <$> get
    guard (l == 1)

readSafe :: (Read a, Alternative m) => String -> m a
readSafe str =
    case readMaybe str of
        Nothing -> empty
        Just n -> pure n

evalRPN :: String -> Maybe Integer
evalRPN str = evalStateT evalRPN' []
    where
        evalRPN' = traverse step (words str) >> oneElementOnStack >> pop
        step "+" = processTops (+)
        step "*" = processTops (*)
        step "-" = processTops (-)
        step t = readSafe t >>= push
        processTops op = flip op <$> pop <*> pop >>= push