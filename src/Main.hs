{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO
import IRC.IRC
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (putMVar, newEmptyMVar)
import Control.Exception (bracket)
import Control.Monad (unless, foldM)
import System.Environment (getArgs)
import Processor (ProcessType(..), UpdateSource, handleIncoming)
import IRC.Display (printCC)
import qualified Config (Config(..), findGroup, readFile')
import qualified IRC.Message as Message (parse)
import qualified IRC.Client as Client

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

start :: Config.Config -> Username -> Oauth -> [Channel] -> IO ()
start cfg username oauth channels =
    bracket (Client.connect "irc.twitch.tv" 6667) hClose $ \h -> do
        let s = Client.State {
            Client.connection = h,
            Client.channels   = [],
            Client.moderators = [],
            Client.config     = cfg
        }
        Client.updateTitle s
        Client.login username oauth s
        state <- foldM Client.joinChannel s channels
        startProcessing state

initFromConfig :: Config.Config -> IO ()
initFromConfig cfg = do
    channels <- getArgs
    case Config.findGroup cfg ["username","oauth"] of
        uname:oauth:_ -> start cfg uname oauth channels
        _ -> printCC cfg
                "~y~Usage~w~||: <username> <oauth key> [channels to join]"

main :: IO ()
main = do
    cfg <- Config.readFile' "settings.cfg"
    args <- getArgs
    case args of
        uname:oauth:channels ->
            start cfg uname oauth channels
        _ -> initFromConfig cfg
