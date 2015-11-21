module IRC.Display (printCC) where

import System.Console.ANSI
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)

-- Prints a string with color formatting.
-- ~COLOR~ specifies a color to use for the next portion of the text,
-- where COLOR is the first letter of the desired color.
--
-- || indicates that color parsing should be stopped and the rest of the line will be printed.
printCC :: String -> IO ()
printCC str = do
    curTime <- getZonedTime
    putStr $ formatTime defaultTimeLocale "[%I:%M:%S] " curTime
    printCC' str

printCC' :: String -> IO ()
printCC' [] = setSGR [Reset]
printCC' ('~':c:'~':xs) = do
    setSGR [SetColor Foreground Dull color]
    printCC' xs
    where color =
            case c of
                'w' -> White
                'g' -> Green
                'c' -> Cyan
                'm' -> Magenta
                'r' -> Red
                'y' -> Yellow
                _   -> White
printCC' ('|':'|':xs) = do
    putStrLn xs
    setSGR [Reset]
printCC' (x:xs) = do
    putChar x
    printCC' xs
