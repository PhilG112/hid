module Ch5.Maybe where

import Text.Read

doubleStrNumber1 :: String -> Maybe Int
doubleStrNumber1 str =
    case readMaybe str of
        Just v -> Just (2 * v)
        Nothing -> Nothing

doubleStrNumber2 :: String -> Maybe Int
doubleStrNumber2 s = fmap (* 2) (readMaybe s)

plusStrNumbers :: String -> String -> Maybe Int
plusStrNumbers s1 s2 = (+) <$> readMaybe s1 <*> readMaybe s2

type Name = String

type Phone = String

type Location = String

type PhoneNumbers = [(Name, Phone)]

type Locations = [(Phone, Location)]

getLocationByName :: PhoneNumbers -> Locations -> Name -> Maybe Location
getLocationByName pNums locs name =
    lookup name pNums >>= \phone -> lookup phone locs