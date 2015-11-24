module Command (process) where

import Control.Monad (foldM)
import Data.List (find)
import Text.Printf (printf)
import IRC.Display (printCC)
import IRC.Client (State(..), joinChannel, leaveChannel, sendMessage)
import Config (Config(..))
import qualified Config (set)

type Name = String
type Usage = String
type NumArgs = Int
type Arguments = [String]
type Action = State -> Arguments -> IO State

type Command = (Name, Usage, NumArgs, Action)

commands :: [Command]
commands = [
    ("join", "<channels>", 1, joinCmd),
    ("leave", "<channels>", 1, leaveCmd),
    ("send", "<channel> <message>", 2, sendCmd),
    ("setcfg", "<key> <value>", 2, setcfgCmd),
    ("mods", "<channel>", 1, modsCmd),
    ("channels", "", 0, channelsCmd),
    ("leaveall", "", 0, leaveallCmd)]

-- Start of command implementations

joinCmd :: Action
joinCmd = foldM joinChannel

leaveCmd :: Action
leaveCmd = foldM leaveChannel

sendCmd :: Action
sendCmd s (chan:msg) = do
    sendMessage chan (unwords msg) s
    return s
sendCmd s _ = return s

setcfgCmd :: Action
setcfgCmd s (name:value:_) = do
    let newCfg =
            Config.set (config s) name value
    return $ s { config = newCfg }
setcfgCmd s _ = return s

modsCmd :: Action
modsCmd s (chan:_) = do
    printCC (config s) $ printf "~r~%d~y~ moderators connected to ~r~||%s"
        (length mods)
        chan
    mapM_ (\(i,x) -> printCC (config s) $ printf "~y~%d.~r~|| %s" i x) mods
    return s
    where mods =
            zip [(1::Int)..]
            . map snd
            . filter (\(c,_) -> c == chan) $ moderators s
modsCmd s _ = return s

channelsCmd :: Action
channelsCmd s _ = do
    printCC (config s) $ printf "~y~Connected to ~r~%d~y~|| channels"
        (length channels')
    mapM_ (\(i,x) -> printCC (config s) $ printf "~y~%d. ~r~||%s" i x) channels'
    return s
    where channels' = zip [(1::Int)..] $ channels s

leaveallCmd :: Action
leaveallCmd s _ = foldM leaveChannel s $ channels s

-- End of command implementations

printCommands :: Config -> IO ()
printCommands cfg = do
    printCC cfg $ printf "~r~%d~y~|| commands" $ length commands'
    mapM_ (\(i, (name, usage, _, _)) ->
        if null usage
            then printCC cfg $ printf "~y~%d. ~r~||%s" i name
            else printCC cfg $ printf "~y~%d. ~r~%s: ~w~||%s" i name usage
        ) commands'
    where commands' = zip [(1::Int)..] commands

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
        match ("commands":_) =
            Right $ do
                printCommands (config state)
                return state
        match (cmd:args) = executeCommand cmd args state
        match [] = Left "No input"
