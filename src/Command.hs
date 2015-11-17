module Command (process) where

import Control.Monad (foldM)
import Data.List (find)
import Text.Printf (printf)
import IRC.Client (State, joinChannel)

type Name = String
type Usage = String
type NumArgs = Int
type Arguments = [String]
type Action = State -> Arguments -> IO State

type Command = (Name, Usage, NumArgs, Action)

commands :: [Command]
commands = [
    ("join", "<channels>", 1, joinCmd)]

joinCmd :: State -> Arguments -> IO State
joinCmd = foldM (flip joinChannel)

findCommand :: Name -> Maybe Command
findCommand name = find (\(n, _, _, _) -> n == name) commands

process :: State -> String -> Either String (IO State)
process state = match . words
    where
        match (cmd:args) =
            case findCommand cmd of
                Just (_, usage, reqArgs, f) ->
                    if reqArgs > length args
                    then Left $ printf $ "Usage: " ++ usage
                    else Right $ f state args
                Nothing -> Left $ "Command not found: " ++ cmd
        match [] = Left "No input"
