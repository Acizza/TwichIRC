module Main where

import Client
import Control.Exception (finally)
import Control.Monad (forever, unless)
import System.IO

processNetworkLoop :: Handle -> IO ()
processNetworkLoop handle = do
    eof <- hIsEOF handle
    unless eof $ do
        line <- hGetLine handle
        unless (null line) $ putStrLn line
        processNetworkLoop handle

main :: IO ()
main = do
    handle <- connect "irc.twitch.tv" 6667
    (login "" "" handle >>= processNetworkLoop) `finally` hClose handle
