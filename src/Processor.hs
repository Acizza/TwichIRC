module Processor
( ProcessType(..)
, UpdateSource
, process
) where

import Control.Concurrent.MVar (MVar, takeMVar)
import IRC.Message (parse)
import IRC.Display (printCC)
import qualified Command as C (process)
import qualified IRC.Client as I (State, process)

data ProcessType =
    IRC String |
    Console String

type UpdateSource = MVar ProcessType

process :: UpdateSource -> I.State -> IO ()
process us state = do
    type' <- takeMVar us
    newS <-
        case type' of
            IRC x -> do
                case parse x of
                    Just x' -> I.process state x'
                    Nothing -> return ()
                return state
            Console x ->
                case C.process state x of
                    Left msg -> do
                        printCC $ "~r~Error:~w~|| " ++ msg
                        return state
                    Right x' -> x'

    process us newS
