module IRC.Client
( State(..)
, connect
, Oauth
, login
, joinChannel
, leaveChannel
, sendMessage
, process
) where

import Control.Exception (finally)
import Control.Monad (unless)
import Data.List (delete)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)
import System.IO
import Text.Printf (printf)
import IRC.Display (printCC)
import IRC.Message (Channel, Username, Message(..))
import Network (HostName, PortNumber, PortID(..), connectTo, withSocketsDo)

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
    return handle

type Oauth = String

login :: Username -> Oauth -> Handle -> IO Handle
login username oauth h = do
    hPutStrLn h $ printf "USER %s 0 * :%s" username username
    hPutStrLn h $ "PASS " ++ oauth
    hPutStrLn h $ "NICK " ++ username
    hPutStrLn h "CAP REQ :twitch.tv/membership"
    return h

joinChannel :: Channel -> State -> IO State
joinChannel channel s = do
    hPutStrLn (connection s) $ "JOIN #" ++ channel
    return $ s { channels = channel : channels s }

leaveChannel :: Channel -> State -> IO State
leaveChannel channel s = do
    hPutStrLn (connection s) $ "PART #" ++ channel
    return $ s { channels = delete channel $ channels s }

sendMessage :: Channel -> String -> State -> IO ()
sendMessage channel msg s =
    hPutStrLn (connection s) $ printf "PRIVMSG #%s :%s" channel msg

process :: State -> Message -> IO ()
process s (Ping content) = hPutStrLn (connection s) $ "PONG " ++ content
process s msg = do
    let str = show msg
    unless (null str) $ do
        curTime <- getZonedTime
        let timeStr = formatTime defaultTimeLocale "[%I:%M:%S]" curTime
        printCC $ printf "~w~%s %s" timeStr str
