module Client
( connect
, login
) where

import Control.Exception (finally)
import Network
import System.IO
import Text.Printf (printf)

connect :: HostName -> PortNumber -> IO Handle
connect hostname port = withSocketsDo $ do
    handle <- connectTo hostname (PortNumber port)
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    return handle

type Username = String
type Oauth = String

login :: Username -> Oauth -> Handle -> IO Handle
login username oauth h = do
    hPutStrLn h $ printf "USER %s 0 * :%s" username username
    hPutStrLn h $ "PASS " ++ oauth
    hPutStrLn h $ "NICK " ++ username
    return h
