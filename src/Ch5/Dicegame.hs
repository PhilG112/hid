module Ch5.Dicegame where

import Control.Monad.RWS
    ( MonadReader (ask),
      MonadState (state),
      MonadWriter (tell),
      RWS,
      replicateM, evalRWS,
    )
import System.Random (StdGen, uniformR, newStdGen)

type Dice = Int

type DiceGame = RWS (Int, Int) [Dice] StdGen

dice :: DiceGame Dice
dice = do
    bs <- ask
    r <- state (uniformR bs)
    tell [r]
    pure r

doubleDice :: DiceGame (Dice, Dice)
doubleDice = (,) <$> dice <*> dice

dices :: Int -> DiceGame [Dice]
dices n = replicateM n dice

diceGame :: DiceGame (Dice, Dice)
diceGame =
    dice
        >> dices 5
        >> replicateM 2 (dices 3)
        >> dices 10
        >> doubleDice

main :: IO ()
main = newStdGen >>= \g -> print $ evalRWS diceGame (1, 6) g