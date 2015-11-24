module IRC.Display (printCC) where

import System.Console.ANSI
import Config (Config, find)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)

-- Prints a string with color formatting.
-- ~COLOR~ specifies a color to use for the next portion of the text,
-- where COLOR is the first letter of the desired color.
--
-- || indicates that color parsing should be stopped and the rest of the line will be printed.
printCC :: Config -> String -> IO ()
printCC cfg str = do
    curTime <- getZonedTime
    putStr $ formatTime defaultTimeLocale "[%I:%M:%S] " curTime
    printCC' cfg str

printCC' :: Config -> String -> IO ()
printCC' _ [] = setSGR [Reset]
printCC' cfg ('~':c:'~':xs) = do
    setSGR [SetColor Foreground Dull color]
    printCC' cfg xs
    where color =
            case c of
                'w' -> tryFind cfg "white" White
                'g' -> tryFind cfg "green" Green
                'c' -> tryFind cfg "cyan" Cyan
                'm' -> tryFind cfg "magenta" Magenta
                'r' -> tryFind cfg "red" Red
                'y' -> tryFind cfg "yellow" Yellow
                _   -> tryFind cfg "white" White
printCC' _ ('|':'|':xs) = do
    putStrLn xs
    setSGR [Reset]
printCC' c (x:xs) = do
    putChar x
    printCC' c xs

tryFind :: Config -> String -> Color -> Color
tryFind cfg name defColor =
    case find cfg ("c." ++ name) of
        Just val ->
            case val of
                "white"   -> White
                "green"   -> Green
                "cyan"    -> Cyan
                "magenta" -> Magenta
                "red"     -> Red
                "yellow"  -> Yellow
                _         -> defColor
        Nothing -> defColor
