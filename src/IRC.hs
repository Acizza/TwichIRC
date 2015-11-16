module IRC
( State(..)
, connect
, Oauth
, login
, joinChannel
, leaveChannel
, process
) where

import Control.Exception (finally)
import Network
import System.IO
import Text.Printf (printf)
import Message (Channel, Username, Message(..))

data State = State {
    connection :: Handle,
    channels   :: [String]
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

joinChannel :: Channel -> Handle -> IO Handle
joinChannel channel h = do
    hPutStrLn h $ "JOIN #" ++ channel
    return h

leaveChannel :: Channel -> Handle -> IO ()
leaveChannel channel h = hPutStrLn h $ "PART #" ++ channel

process :: State -> Message -> IO ()
process s (Ping content) = hPutStrLn (connection s) $ "PONG " ++ content
process s msg =
    print msg
