module Ch5.Sumlist where
import Control.Monad.State
import Data.Foldable (traverse_)

addItem :: Integer -> State Integer ()
addItem i = do
    s <- get
    put (s + i)

addItem' :: Integer -> State Integer ()
addItem' i = modify' (\s -> s + i)

sumList :: [Integer] -> State Integer ()
sumList xs = traverse_ addItem xs