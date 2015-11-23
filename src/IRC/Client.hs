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
import Data.List (delete, isPrefixOf)
import Text.Printf (printf)
import System.Console.ANSI (setTitle)
import Config (Config(..))

data State = State {
    connection :: Handle,
    channels   :: [Channel],
    moderators :: [(Channel, String)],
    config     :: Config
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
    let next = state {
        channels = delete channel $ channels state
    } {
        moderators = filter (\(c,_) -> c /= channel) $ moderators state
    }
    updateTitle next
    return next

sendMessage :: Channel -> String -> State -> IO ()
sendMessage channel msg s =
    sendLine s $ printf "PRIVMSG #%s :%s" channel msg

display :: State -> Message -> String
display s (Message ch uname msg)
    | "\SOHACTION" `isPrefixOf` msg  =
        printf "~g~<%s> ~r~%s ~y~||%s" ch uname
            (init . drop (length "\SOHACTION ") $ msg)
    | uname == ch                    = printf "~g~<%s> ~y~[B] ~c~%s~w~||: %s" ch uname msg
    | (ch,uname) `elem` moderators s = printf "~g~<%s> ~y~[M] ~c~%s~w~||: %s" ch uname msg
    | uname == "twitchnotify"        = -- Subscriptions
        let (user,msg') = span (/=' ') msg
        in printf "~g~<%s> ~r~%s ~y~||%s" ch user (tail msg')
    | otherwise                      = printf "~g~<%s> ~c~%s~w~||: %s" ch uname msg
display _ (Join ch uname)            = printf "~g~<%s> ~c~%s ~y~||joined" ch uname
display _ (Leave ch uname)           = printf "~g~<%s> ~c~%s ~y~||left" ch uname
display _ (ModeratorJoin ch uname)   = printf "~g~<%s> ~y~Moderator ~r~%s ~y~||joined" ch uname
display _ (ModeratorLeave ch uname)  = printf "~g~<%s> ~y~Moderator ~r~%s ~y~||left" ch uname
display _ (Ping _)                   = ""
display _ (Login (Success uname))    = printf "~g~Logged in as ~c~||%s" uname
display _ (Login (Failure rsn))      = printf "~r~Login failed: ~w~||%s" rsn

process :: State -> Message -> IO State
process s m@(ModeratorJoin ch uname) = do
    printCC (config s) $ display s m
    return $ s { moderators = (ch,uname):moderators s }
process s m@(ModeratorLeave ch uname) = do
    printCC (config s) $ display s m
    return $ s { moderators = delete (ch,uname) $ moderators s}
process s (Ping content) = do
    sendLine s $ "PONG " ++ content
    return s
process s m = do
    printCC (config s) $ display s m
    return s
