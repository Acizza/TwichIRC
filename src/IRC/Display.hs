module IRC.Display
( printCC
, printCCError
) where

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

printCCError :: Config -> String -> IO ()
printCCError cfg str = printCC cfg $ "~r~Error~w~||: " ++ str

printCC' :: Config -> String -> IO ()
printCC' _ [] = setSGR [Reset]
printCC' cfg ('~':c:'~':xs) = do
    setSGR [SetColor Foreground Dull color]
    printCC' cfg xs
    where color =
            case c of
                'b' -> tryFind cfg "black" Black
                'r' -> tryFind cfg "red" Red
                'g' -> tryFind cfg "green" Green
                'y' -> tryFind cfg "yellow" Yellow
                'B' -> tryFind cfg "blue" Blue
                'm' -> tryFind cfg "magenta" Magenta
                'c' -> tryFind cfg "cyan" Cyan
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
                "black"   -> Black
                "red"     -> Red
                "green"   -> Green
                "yellow"  -> Yellow
                "blue"    -> Blue
                "magenta" -> Magenta
                "cyan"    -> Cyan
                "white"   -> White
                _         -> defColor
        Nothing -> defColor
