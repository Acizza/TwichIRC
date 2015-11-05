module Main where

import Client
import Control.Monad (forever, unless)
import System.IO

startMessageLoop :: Handle -> IO ()
startMessageLoop handle = forever $ do
    line <- hGetLine handle
    unless (null line) $ putStrLn line

main :: IO ()
main = do
    handle <- connect "irc.twitch.tv" 6667
    login "" "" handle
        >>= startMessageLoop
    hClose handle
