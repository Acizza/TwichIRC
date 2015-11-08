module Main where

import Client
import Control.Exception (bracket)
import Control.Monad (forever, unless)
import System.IO
import Message

preprocessLine :: Maybe Message -> Handle -> IO ()
preprocessLine (Just (Ping content)) h = hPutStrLn h $ "PONG " ++ content
preprocessLine _ _ = return ()

processNetworkLoop :: Handle -> IO ()
processNetworkLoop handle = do
    eof <- hIsEOF handle
    unless eof $ do
        line <- hGetLine handle
        unless (null line) $ do
            let result = parse line
            preprocessLine result handle
            print result

        processNetworkLoop handle

main :: IO ()
main =
    bracket (connect "irc.twitch.tv" 6667) hClose $ \h ->
        login "" "" h
            >>= joinChannel ""
            >>= processNetworkLoop
