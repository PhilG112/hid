{-# LANGUAGE NamedFieldPuns #-}

module Ch5.Reader where

import Control.Monad.Reader

data Config = Config
    { verbose :: Bool
    }

type ConfigM = Reader Config

getConfiguration :: IO Config
getConfiguration = return Config {verbose = True}

main :: IO ()
main = do
    config <- getConfiguration
    let result = runReader work config
    print result

silent :: Config -> Config
silent config = config {verbose = False}

work :: ConfigM ()
work = do
    doSomething

doSomething :: ConfigM ()
doSomething = do
    doSomethingSpecial

doSomethingSpecial :: ConfigM ()
doSomethingSpecial = do
    vrb <- asks verbose
    when vrb beVerbose

beVerbose :: ConfigM ()
beVerbose = pure ()

doSomethingSpecialSilently :: ConfigM ()
doSomethingSpecialSilently = local silent doSomethingSpecial