module Config
( Entry(..)
, Config(..)
, find
, parse
) where

import Text.Regex.PCRE
import qualified Data.List as L (find)

type Name = String
type Value = String

data Entry = Entry Name Value deriving (Show)
newtype Config = Config [Entry] deriving(Show)

find :: Config -> Name -> Maybe Value
find (Config cfg) name =
    case result of
        Just (Entry _ v) -> Just v
        Nothing -> Nothing
    where result =
            L.find (\(Entry n _) -> n == name) cfg

parse :: String -> Config
parse content =
    Config $ map (\(_:f:s:_) -> Entry f s) matches
    where matches =
            content =~ "^([^#].+?):\\s*(.+)" :: [[String]]
