{-# LANGUAGE ScopedTypeVariables #-}

module Command (process) where

import Control.Exception (try, SomeException)
import Control.Monad (foldM)
import Data.List (find)
import Text.Printf (printf)
import IRC.Display (printCC, printCCError)
import IRC.Client (State(..), joinChannel, leaveChannel, sendMessage)
import Config (Config(..))
import qualified Config (set, path)

type Name = String
type Usage = String
type NumArgs = Int
type Arguments = [String]
type Action = State -> Arguments -> IO State

data Command = Command Name Usage NumArgs Action

commands :: [Command]
commands = [
    Command "join" "<channels>" 1 joinCmd,
    Command "leave" "<channels>" 1 leaveCmd,
    Command "send" "<channel> <message>" 2 sendCmd,
    Command "mods" "<channel>" 1 modsCmd,
    Command "channels" "" 0 channelsCmd,
    Command "leaveall" "" 0 leaveallCmd,
    Command "setcfg" "<key> <value>" 2 setcfgCmd,
    Command "savecfg" "" 0 savecfgCmd]

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

modsCmd :: Action
modsCmd s (chan:_) = do
    printCC (config s) $ printf "~r~%d~y~ moderators connected to ~r~||%s"
        (length mods)
        chan
    mapM_ (\(i,x) -> printCC (config s) $ printf "~y~%d.~r~|| %s" i x) mods
    return s
    where mods =
            zip [(1::Int)..] .
            map snd .
            filter (\(c,_) -> c == chan) $ moderators s
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

setcfgCmd :: Action
setcfgCmd s (name:value:_) = do
    let newCfg =
            Config.set (config s) name value
    return $ s { config = newCfg }
setcfgCmd s _ = return s

savecfgCmd :: Action
savecfgCmd s _ = do
    result <- try $ writeFile Config.path settings
    case result of
        Left (e::SomeException) -> printCCError cfg $ show e
        Right _ -> printCC cfg "~y~Config update:~g~|| Success"
    return s
    where
        cfg = config s
        settings = show cfg

-- End of command implementations

printCommands :: Config -> IO ()
printCommands cfg = do
    printCC cfg $ printf "~r~%d~y~|| commands" $ length commands'
    mapM_ (\(i, Command name usage _ _) ->
        if null usage
            then printCC cfg $ printf "~y~%d. ~r~||%s" i name
            else printCC cfg $ printf "~y~%d. ~r~%s: ~w~||%s" i name usage
        ) commands'
    where commands' = zip [(1::Int)..] commands

findCommand :: Name -> Maybe Command
findCommand name = find (\(Command n _ _ _) -> n == name) commands

executeCommand :: Name -> Arguments -> State -> Either String (IO State)
executeCommand name args state =
    case findCommand name of
        Just (Command _ usage nArgs f) ->
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
