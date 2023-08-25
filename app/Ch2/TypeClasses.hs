{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch2.TypeClasses (rotateFromFile, orientFromFile) where
import Fmt ( Buildable(..), Builder, (+||), unwordsF, nameF )
import Fmt.Internal.Core

data Direction = North | East | South | West
    deriving (Eq, Enum, Bounded, Show, CyclicEnum, Read)

data Turn = TNone | TLeft | TRight | TAround
    deriving (Eq, Enum, Bounded, Show, Read)

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise = pred d

    csucc :: a -> a
    csucc d
        | d == maxBound = minBound
        | otherwise = succ d

instance Semigroup Turn where
    (<>) :: Turn -> Turn -> Turn
    TNone <> t = t
    TLeft <> TLeft = TAround
    TLeft <> TRight = TNone
    TLeft <> TAround = TRight
    TRight <> TRight = TAround
    TRight <> TAround = TLeft
    TAround <> TAround = TNone
    t1 <> t2 = t2 <> t1

instance Monoid Turn where
    mempty :: Turn
    mempty = TNone

instance Buildable Direction where
    build :: Direction -> Builder
    build North = "N"
    build East = "E"
    build South = "S"
    build West = "W"

instance Buildable Turn where
    build :: Turn -> Builder
    build TNone = "--"
    build TLeft = "<-"
    build TRight = "->"
    build TAround = "||"

rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight = csucc
rotate TAround = cpred . cpred

orient :: Direction -> Direction -> Turn
orient start end = head $ filter (\t -> rotate t start == end) every

rotateMany :: Direction -> [Turn] -> Direction
-- rotateMany = foldl (flip rotate) - If you want to be a smarty pants
rotateMany start turns = foldl (\s t -> rotate t s) start turns

-- After implementing Semigroup and Monoid - MIND BLOWN
rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' start turns = rotate (mconcat turns) start 

rotateManySteps :: Direction -> [Turn] -> [Direction]
-- rotateManySteps = scanl (flip rotate) - If you want to be a smarty pants
rotateManySteps d turns = scanl (\s t -> rotate t s) d turns

orientMany :: [Direction] -> [Turn]
orientMany xs@(_:_:_) = zipWith orient xs (tail xs)
orientMany _ = []

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile start fileName = do
    f <- readFile fileName
    let turns = map read $ lines f
        finalDirection = rotateMany start turns
        directions = rotateManySteps start turns
    fmtLn $ "Final direction: "+||finalDirection||+""
    fmt $ nameF "Intermediate directions" (unwordsF directions)

orientFromFile :: FilePath -> IO ()
orientFromFile fname = do
    f <- readFile fname
    let dirs = map read $ lines f
        allTurns = orientMany dirs
    fmt $ nameF "All turns" (unwordsF allTurns) 

every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound