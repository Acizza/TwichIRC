module Message
( Channel
, Username
, Message(..)
, parse
) where

import Data.Maybe (maybe)

type Channel = String
type Username = String

data Message =
    Join Channel Username |
    Leave Channel Username |
    Message Channel Username String
    deriving (Show)

-- Safe version of !!
(!!!) :: [a] -> Int -> Maybe a
(!!!) list idx
    | idx >= length list = Nothing
    | otherwise          = Just $ list !! idx

parse :: String -> Maybe Message
parse str =
    code >>= \c ->
        case c of
            "JOIN"    -> Just $ Join channel username
            "PART"    -> Just $ Leave channel username
            "PRIVMSG" -> Just $ Message channel username "Testing"
            _ -> Nothing
    where
        sections = words str
        code = sections !!! 1
        username = maybe "ERROR" (takeWhile (/='!') . drop 1) (sections !!! 0)
        channel = maybe "ERROR" (drop 1) (sections !!! 2)
