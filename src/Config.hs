{-# LANGUAGE ScopedTypeVariables #-}

module Config
( Entry(..)
, Config(..)
, parse
, readFile'
, find
, findGroup
) where

import Text.Regex.PCRE
import Data.Maybe (mapMaybe)
import Control.Exception (try, SomeException)
import qualified Data.List as L (find)

type Name = String
type Value = String

data Entry = Entry Name Value
newtype Config = Config [Entry]

parse :: String -> Config
parse content =
    Config $ map (\(_:f:s:_) -> Entry f s) matches
    where matches =
            content =~ "^([^#].+?):\\s*(.+)" :: [[String]]

readFile' :: String -> IO Config
readFile' path = do
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
