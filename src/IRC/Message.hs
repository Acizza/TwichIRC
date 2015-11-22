module IRC.Message
( Result(..)
, Message(..)
, parse
) where

import IRC.IRC
import Data.List.Split (splitOn)

type Reason = String

data Result = Success Username | Failure Reason
    deriving (Show)

data Message =
    Message Channel Username String |
    Join Channel Username |
    Leave Channel Username |
    Ping String |
    Login Result

-- Safe version of !!
(!!!) :: [a] -> Int -> Maybe a
(!!!) list idx
    | idx >= length list = Nothing
    | otherwise          = Just $ list !! idx

getCode :: [String] -> Maybe String
getCode ("PING":_) = Just "PING"
getCode (_:code:_) = Just code
getCode _          = Nothing

parse :: String -> Maybe Message
parse str =
    code >>= \c -> case c of
        "PRIVMSG" -> Just $ Message channel username (tail . dropWhile (/=':') . tail $ str)
        "JOIN"    -> Just $ Join channel username
        "PART"    -> Just $ Leave channel username
        "PING"    -> Just $ Ping $ drop (length "PING ") str
        "004"     -> Just $ Login (Success $ sections !! 2)
        "NOTICE"  -> Just $ Login (Failure $ splitOn " :" str !! 1)
        _         -> Nothing
    where
        sections = words str
        code = getCode sections
        username = maybe "ERROR" (tail . takeWhile (/='!')) (sections !!! 0)
        channel = maybe "ERROR" tail (sections !!! 2)
