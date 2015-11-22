module Processor
( ProcessType(..)
, UpdateSource
, handleIncoming
) where

import Control.Concurrent.MVar (MVar, takeMVar)
import IRC.Message (Message)
import IRC.Display (printCC)
import qualified Command (process)
import qualified IRC.Client as Client (State, process)

data ProcessType =
    IRC Message |
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
                        printCC $ "~r~Error:~w~|| " ++ msg
                        return state

    handleIncoming us newState
