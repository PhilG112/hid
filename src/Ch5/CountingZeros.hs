module Ch5.CountingZeros where

import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Foldable (traverse_)
import Data.STRef (modifySTRef', newSTRef, readSTRef)

countZerosST :: [Int] -> Int
countZerosST xs = runST $ do
    c <- newSTRef 0
    traverse_ (\x -> when (x == 0) $ inc c) xs
    readSTRef c
    where
        inc c = modifySTRef' c (+ 1)