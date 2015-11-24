{-# LANGUAGE ScopedTypeVariables #-}

module Config
( Entry(..)
, Config(..)
, path
, parse
, readFile'
, find
, findGroup
, set
) where

import Text.Regex.PCRE
import Data.Maybe (mapMaybe)
import Control.Exception (try, SomeException)
import qualified Data.List as L (find)

type Name = String
type Value = String

data Entry = Entry Name Value

instance Show Entry where
    show (Entry n v) = n ++ ": " ++ v

newtype Config = Config [Entry]

path :: String
path = "settings.cfg"

parse :: String -> Config
parse content =
    Config $ map (\(_:f:s:_) -> Entry f s) matches
    where matches =
            content =~ "^([^#].+?):\\s*(.+)" :: [[String]]

readFile' :: IO Config
readFile' = do
    result <- try $ readFile path
    return $
        case result of
            Left (_::SomeException) -> Config []
            Right x -> parse x

find :: Config -> Name -> Maybe Value
find (Config cfg) name =
    case result of
        Just (Entry _ v) -> Just v
        Nothing -> Nothing
    where result =
            L.find (\(Entry n _) -> n == name) cfg

findGroup :: Config -> [String] -> [Value]
findGroup cfg = mapMaybe (find cfg)

-- Replaces all instances of name with value.
-- Adds the Entry if it doesn't exist.
set :: Config -> Name -> Value -> Config
set cfg@(Config cfg') name value =
    Config $
        case find cfg name of
            Just _ -> replace name value
            Nothing -> Entry name value : cfg'
    where replace name' value' =
            map (\e@(Entry n _) ->
                if n == name'
                    then Entry n value'
                    else e
            ) cfg'
