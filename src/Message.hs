module Message
( Channel
, Username
, Result(..)
, Message(..)
, parse
) where

import Control.Monad (unless)
import Data.Maybe (maybe)
import Data.List.Split (splitOn)
import System.IO (hPutStrLn, Handle)

type Channel = String
type Username = String
type Reason = String

data Result = Success Username | Failure Reason
    deriving (Show)

data Message =
    Message Channel Username String |
    Join Channel Username |
    Leave Channel Username |
    Ping String |
    Login Result
    deriving (Show)

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
    code >>= \c ->
        case c of
            "PRIVMSG" -> Just $ Message channel username (drop 1 . dropWhile (/=':') . drop 1 $ str)
            "JOIN"    -> Just $ Join channel username
            "PART"    -> Just $ Leave channel username
            "PING"    -> Just $ Ping $ drop (length "PING ") str
            "004"     -> Just $ Login (Success $ sections !! 2)
            "NOTICE"  -> Just $ Login (Failure $ splitOn " :" str !! 1)
            _ -> Nothing
    where
        sections = words str
        code = getCode sections
        username = maybe "ERROR" (drop 1 . takeWhile (/='!')) (sections !!! 0)
        channel = maybe "ERROR" (drop 1) (sections !!! 2)
