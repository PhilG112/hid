module Main (main) where

import Ch2.TypeClasses (orientFromFile, rotateFromFile)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-r", fname, dir] -> rotateFromFile (read dir) fname
        ["-o", fname] -> orientFromFile fname
        _ ->
            putStrLn $
                "Usage : hid -o fileName\n"
                    ++ "        hid -r fileName direction"
