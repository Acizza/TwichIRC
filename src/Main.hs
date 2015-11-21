module Main where

import System.IO
import IRC.IRC
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (putMVar, newEmptyMVar)
import Control.Exception (bracket)
import Control.Monad (unless)
import Processor (ProcessType(..), UpdateSource, handleIncoming)
import qualified IRC.Message as Message (parse)
import qualified IRC.Client as Client

initClient :: Handle -> Username -> Oauth -> IO Client.State
initClient conn username oauth = do
    let state = Client.State {
        Client.connection = conn,
        Client.channels   = [],
        Client.moderators = []
    }
    Client.login username oauth state
    Client.joinChannel state ""

startProcessing :: Client.State -> IO ()
startProcessing state = do
    updater <- newEmptyMVar
    _ <- forkIO $ handleIncoming updater state
    _ <- forkIO $ processNetwork updater (Client.connection state)
    processConsole updater

processNetwork :: UpdateSource -> Handle -> IO ()
processNetwork us handle = do
    eof <- hIsEOF handle
    op  <- hIsClosed handle
    unless (eof || op) $ do
        line <- hGetLine handle
        unless (null line) $
            case Message.parse line of
                Just x -> putMVar us (IRC x)
                Nothing -> return ()

        processNetwork us handle

processConsole :: UpdateSource -> IO ()
processConsole us = do
    line <- getLine
    putMVar us (Console line)
    processConsole us

main :: IO ()
main =
    bracket (Client.connect "irc.twitch.tv" 6667) hClose $ \h -> do
        state <- initClient h "" ""
        startProcessing state
