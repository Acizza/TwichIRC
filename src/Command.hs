module Command (process) where

import Control.Monad (foldM)
import Data.List (find)
import Text.Printf (printf)
import IRC.Display (printCC)
import IRC.Client (State, joinChannel, leaveChannel)

type Name = String
type Usage = String
type NumArgs = Int
type Arguments = [String]
type Action = State -> Arguments -> IO State

type Command = (Name, Usage, NumArgs, Action)

commands :: [Command]
commands = [
    ("join", "<channels>", 1, joinCmd),
    ("leave", "<channels>", 1, leaveCmd)]

joinCmd = foldM (flip joinChannel)
leaveCmd = foldM (flip leaveChannel)

printCommands :: IO ()
printCommands =
    mapM_ (\(name, usage, _, _) ->
        printCC $ printf "~r~%s: ~w~%s\n" name usage)
        commands

findCommand :: Name -> Maybe Command
findCommand name = find (\(n, _, _, _) -> n == name) commands

executeCommand :: Name -> Arguments -> State -> Either String (IO State)
executeCommand name args state =
    case findCommand name of
        Just (_, usage, nArgs, f) ->
            if nArgs > length args
            then Left $ printf $ "Usage: " ++ usage
            else Right $ f state args
        Nothing -> Left $ "Command not found: " ++ name

process :: State -> String -> Either String (IO State)
process state = match . words
    where
        match ("commands":args) =
            Right $ do
                printCommands
                return state
        match (cmd:args) = executeCommand cmd args state
        match [] = Left "No input"
