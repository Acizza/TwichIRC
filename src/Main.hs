{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.IO
import IRC.IRC
import Control.Exception (bracket)
import Control.Monad (foldM)
import System.Environment (getArgs)
import Processor (startProcessing)
import IRC.Display (printCC)
import qualified Config (Config(..), findGroup, readFile')
import qualified IRC.Client as C

start :: Config.Config -> Username -> Oauth -> [Channel] -> IO ()
start cfg username oauth channels =
    bracket (C.connect "irc.twitch.tv" 6667) hClose $ \h -> do
        let s = C.State {
            C.connection = h,
            C.channels   = [],
            C.moderators = [],
            C.config     = cfg
        }
        C.updateTitle s
        C.login username oauth s
        state <- foldM C.joinChannel s channels
        startProcessing state

initFromConfig :: Config.Config -> IO ()
initFromConfig cfg = do
    channels <- getArgs
    case Config.findGroup cfg ["username","oauth"] of
        uname:oauth:_ -> start cfg uname oauth channels
        _ -> printCC cfg
                "~r~Usage~w~||: <username> <oauth key> [channels to join]"

main :: IO ()
main = do
    cfg <- Config.readFile'
    args <- getArgs
    case args of
        uname:oauth:channels ->
            start cfg uname oauth channels
        _ -> initFromConfig cfg
