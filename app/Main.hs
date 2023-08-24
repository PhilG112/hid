module Main where

import Ch2.TypeClasses (rotateFromFile, orientFromFile)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    f <- readFile $ head args
    putStrLn $ map read $ lines f
    -- case args of
    --     ["-r", fname, dir] -> rotateFromFile (read dir) fname
    --     ["-o", fname] -> orientFromFile fname
    --     _ -> putStrLn $ "Usage : hid -o fileName\n" ++
    --                     "        hid -r fileName direction"
