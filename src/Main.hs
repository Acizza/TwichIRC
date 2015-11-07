module Main where

import Client
import Control.Exception (bracket)
import Control.Monad (forever, unless)
import System.IO

import Message

processNetworkLoop :: Handle -> IO ()
processNetworkLoop handle = do
    eof <- hIsEOF handle
    unless eof $ do
        line <- hGetLine handle
        unless (null line) $ print (parse line)
        processNetworkLoop handle

main :: IO ()
main =
    bracket (connect "irc.twitch.tv" 6667) hClose $ \h ->
        login "" "" h
            >>= joinChannel ""
            >>= processNetworkLoop
