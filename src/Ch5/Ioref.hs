module Ch5.Ioref where

import Control.Monad.Extra (ifM, whenM, zipWithM_)
import Data.Foldable (traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef, IORef)
import System.Directory.Extra (doesDirectoryExist, listContents)
import System.Environment (getArgs)

fileCount :: FilePath -> IO Int
fileCount fpath = do
    counter <- newIORef 0
    whenM (doesDirectoryExist fpath) $ go counter fpath
    readIORef counter
    where
        go :: Num a => IORef a -> FilePath -> IO ()
        go cnt fp = listContents fp >>= traverse_ (processEntry cnt)

        processEntry :: Num a => IORef a -> FilePath -> IO ()
        processEntry cnt fp = ifM (doesDirectoryExist fp) (go cnt fp) (inc cnt)

        inc :: Num a => IORef a -> IO ()
        inc cnt = modifyIORef' cnt (+ 1)

main :: IO ()
main = do
    args <- getArgs
    xs <- traverse fileCount args
    zipWithM_ printEntry args xs
    where
        printEntry fp n = putStrLn (show n ++ "\t" ++ fp)