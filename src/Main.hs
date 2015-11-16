module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (putMVar, newEmptyMVar)
import Control.Exception (bracket)
import Control.Monad (forever, unless)
import System.IO
import Client.Message (Message(..), Username)
import Processor (ProcessType(..), UpdateSource, process)
import qualified Client.IRC as I

processNetwork :: UpdateSource -> Handle -> IO ()
processNetwork us handle = do
    eof <- hIsEOF handle
    unless eof $ do
        line <- hGetLine handle
        unless (null line) $ putMVar us (IRC line)
        processNetwork us handle

initClient :: Handle -> UpdateSource -> Username -> I.Oauth -> IO ()
initClient h var username oauth =
    I.login username oauth h
        >>= I.joinChannel ""
        >>= processNetwork var

processConsole :: UpdateSource -> IO ()
processConsole us = do
    line <- getLine
    putMVar us (Console line)
    processConsole us

main :: IO ()
main = do
    state <- newEmptyMVar
    bracket (I.connect "irc.twitch.tv" 6667) hClose $ \h -> do
        forkIO $ process state $ I.State h []
        forkIO $ initClient h state "" ""
        processConsole state
