module IRC.Client
( State(..)
, updateTitle
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
import IRC.Message (Result(..), Message(..))
import Network (HostName, PortNumber, PortID(..), connectTo, withSocketsDo)
import Data.List (delete)
import Text.Printf (printf)
import System.Console.ANSI (setTitle)

data State = State {
    connection :: Handle,
    channels   :: [Channel],
    moderators :: [(Channel, String)]
}

updateTitle :: State -> IO ()
updateTitle state =
    setTitle $ printf "Twirc (%d)" nChannels
    where nChannels = length (channels state)

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
    let next = state { channels = channel:channels state }
    updateTitle next
    return next

leaveChannel :: State -> Channel -> IO State
leaveChannel state channel = do
    sendLine state $ "PART #" ++ channel
    let next = state { channels = delete channel $ channels state }
    updateTitle next
    return next

sendMessage :: Channel -> String -> State -> IO ()
sendMessage channel msg s =
    sendLine s $ printf "PRIVMSG #%s :%s" channel msg

process :: State -> Message -> IO ()
process s (Message ch uname msg)
    | uname == ch                    = printCC $ printf "~w~[B] ~g~<%s> ~c~%s~w~||: %s" ch uname msg
    | (ch,uname) `elem` moderators s = printCC $ printf "[M] ~g~<%s> ~c~%s~w~||: %s" ch uname msg
    | otherwise                      = printCC $ printf "~g~<%s> ~c~%s~w~||: %s" ch uname msg
process _ (Join ch uname)            = printCC $ printf "~g~<%s> ~c~%s ~m~||joined" ch uname
process _ (Leave ch uname)           = printCC $ printf "~g~<%s> ~c~%s ~m~||left" ch uname
process s (Ping content)             = sendLine s $ "PONG " ++ content
process _ (Login (Success uname))    = printCC $ printf "~g~Logged in as ~c~||%s" uname
process _ (Login (Failure rsn))      = printCC $ printf "~r~Login failed: ~w~||%s" rsn
