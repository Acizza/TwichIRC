module Config where

import Text.Regex.PCRE

type Name = String
type Value = String

data Entry = Entry Name Value deriving (Show)
newtype Config = Config [Entry] deriving(Show)

parse :: String -> Config
parse content =
    Config $ map (\(_:f:s:_) -> Entry f s) matches
    where matches =
            content =~ "(.+?):\\s*(.+)" :: [[String]]
