{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Ch5.GenSQL where

import Control.Monad.Writer (MonadWriter (tell), Writer)
import Data.Text (Text, splitOn)
import qualified Data.Text as T

type SQL = Text

data ErrorMsg = WrongFormat Int Text
    deriving (Show)

genSQL txt = traverse processLine (zip [1 ..] $ T.lines txt)

genInsert :: Text -> Text -> Text
genInsert s1 s2 =
    "INSERT INTO ITEMS VALUES('" <> s1 <> "','" <> s2 <> "')'\n"

processLine :: (Int, Text) -> Writer [ErrorMsg] SQL
processLine (_, splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2
processLine (i, s) = tell [WrongFormat i s] >> pure ""