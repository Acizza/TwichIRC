module IRC.Client
( State(..)
, connect
, login
, joinChannel
, leaveChannel
, sendMessage
, process
) where

import System.IO
import IRC.IRC
import IRC.Display (printCC)
import IRC.Message (Message(..))
import Network (HostName, PortNumber, PortID(..), connectTo, withSocketsDo)
import Control.Monad (unless)
import Data.List (delete)
import Text.Printf (printf)

data State = State {
    connection :: Handle,
    channels   :: [Channel],
    moderators :: [(Channel, String)]
} deriving (Show)

connect :: HostName -> PortNumber -> IO Handle
connect hostname port = withSocketsDo $ do
    handle <- connectTo hostname (PortNumber port)
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    hSetEncoding handle utf8
    return handle

sendLine :: State -> String -> IO ()
sendLine s = hPutStrLn (connection s)

login :: Username -> Oauth -> State -> IO ()
login username oauth s = do
    sendLine s $ printf "USER %s 0 * :%s" username username
    sendLine s $ "PASS " ++ oauth
    sendLine s $ "NICK " ++ username
    sendLine s "CAP REQ :twitch.tv/membership"

joinChannel :: State -> Channel -> IO State
joinChannel state channel = do
    sendLine state $ "JOIN #" ++ channel
    return $ state { channels = channel : channels state }

leaveChannel :: State -> Channel -> IO State
leaveChannel state channel = do
    sendLine state $ "PART #" ++ channel
    return $ state { channels = delete channel $ channels state }

sendMessage :: Channel -> String -> State -> IO ()
sendMessage channel msg s =
    sendLine s $ printf "PRIVMSG #%s :%s" channel msg

process :: State -> Message -> IO ()
process s (Ping content) =
    sendLine s $ "PONG " ++ content
process _ msg = do
    let str = show msg
    unless (null str) $ printCC str
