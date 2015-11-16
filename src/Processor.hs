module Processor
( ProcessType(..)
, UpdateSource
, process
) where

import Control.Concurrent.MVar (MVar, takeMVar)
import Message (parse, Message(..))
import qualified IRC as I (State, process)

data ProcessType =
    IRC String |
    Console String

type UpdateSource = MVar ProcessType

process :: UpdateSource -> I.State -> IO ()
process us state = do
    type' <- takeMVar us
    case type' of
        IRC x ->
            case parse x of
                Just x' -> I.process state x'
                Nothing -> return ()
        Console x ->
            putStrLn $ "Console: " ++ x

    process us state
