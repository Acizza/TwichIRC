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
-- Skip colon at the start of messages to avoid matching it later
parse (_:str) =
    code >>= \c ->
        case c of
            "JOIN"    -> Just $ Join channel username
            "PART"    -> Just $ Leave channel username
            "PRIVMSG" -> Just $ Message channel username (drop 1 $ dropWhile (/=':') str)
            _ -> Nothing
    where
        sections = words str
        code = sections !!! 1
        username = maybe "ERROR" (takeWhile (/='!')) (sections !!! 0)
        channel = maybe "ERROR" (drop 1) (sections !!! 2)
parse _ = Nothing
