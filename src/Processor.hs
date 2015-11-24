module Processor
( ProcessType(..)
, UpdateSource
, startProcessing
) where

import System.IO
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar, newEmptyMVar)
import Control.Monad (unless)
import qualified IRC.Message as Message (Message, parse)
import IRC.Display (printCC)
import qualified Command (process)
import qualified IRC.Client as Client (State(..), process)

data ProcessType =
    IRC Message.Message |
    Console String

type UpdateSource = MVar ProcessType

handleIncoming :: UpdateSource -> Client.State -> IO ()
handleIncoming us state = do
    type' <- takeMVar us
    newState <-
        case type' of
            IRC x -> Client.process state x
            Console x ->
                case Command.process state x of
                    Right x' -> x'
                    Left msg -> do
                        printCC (Client.config state) $ "~r~Error:~w~|| " ++ msg
                        return state

    handleIncoming us newState

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
