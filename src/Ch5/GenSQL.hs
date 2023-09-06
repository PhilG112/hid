{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Ch5.GenSQL where

import Control.Monad.Writer (MonadWriter (tell), Writer)
import Data.Text (Text, splitOn)

type SQL = Text

data ErrorMsg = WrongFormat Int Text
    deriving (Show)

genSQL :: Text -> Writer [ErrorMsg] SQL
genSQL _ = _

genInsert :: Text -> Text -> Text
genInsert s1 s2 =
    "INSERT INTO ITEMS VALUES('" <> s1 <> "','" <> s2 <> "')'\n"

processLine :: (Int, Text) -> Writer [ErrorMsg] SQL
processLine (_, splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2
processLine (i, s) = tell [WrongFormat i s] >> pure ""