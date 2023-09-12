{-# LANGUAGE InstanceSigs #-}

module Ch5.Weapon where

import Control.Monad.State
import Data.List
import System.Random (StdGen, Uniform, UniformRange, newStdGen, uniform)
import System.Random.Stateful (StatefulGen, Uniform (uniformM), UniformRange (uniformRM))

data Weapon = Rock | Paper | Scissors
    deriving (Show, Eq, Enum, Bounded)

data Winner = First | Second | Draw
    deriving (Show, Eq, Ord)

winner :: (Weapon, Weapon) -> Winner
winner (Paper, Rock) = First
winner (Scissors, Paper) = First
winner (Rock, Scissors) = First
winner (w1, w2)
    | w1 == w2 = Draw
    | otherwise = Second

instance UniformRange Weapon where
    uniformRM :: StatefulGen g m => (Weapon, Weapon) -> g -> m Weapon
    uniformRM (lo, hi) rng = do
        res <- uniformRM (fromEnum lo, fromEnum hi) rng
        return $ toEnum res

instance Uniform Weapon where
    uniformM :: StatefulGen g m => g -> m Weapon
    uniformM rng = uniformRM (minBound, maxBound) rng

randomWeapon :: State StdGen Weapon
randomWeapon = state uniform

gameRound :: State StdGen (Weapon, Weapon)
gameRound = fmap (,) randomWeapon <*> randomWeapon

game :: Int -> State StdGen [(Winner, Int)]
game n = counts <$> replicateM n (winner <$> gameRound)
    where
        counts :: [Winner] -> [(Winner, Int)]
        counts xs = map headLength $ group $ sort xs

        headLength :: [Winner] -> (Winner, Int)
        headLength xs@(x : _) = (x, length xs)

main :: IO ()
main = do
    g <- newStdGen
    let r = evalState (game 10) g
    print r