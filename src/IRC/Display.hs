module IRC.Display
( printCC
, printCCError
) where

import System.Console.ANSI
import Config (Config, tryFind)
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
    setSGR [SetColor Foreground (findIntensity cfg) color]
    printCC' cfg xs
    where color =
            case c of
                'b' -> findColor cfg "black" Black
                'r' -> findColor cfg "red" Red
                'g' -> findColor cfg "green" Green
                'y' -> findColor cfg "yellow" Yellow
                'B' -> findColor cfg "blue" Blue
                'm' -> findColor cfg "magenta" Magenta
                'c' -> findColor cfg "cyan" Cyan
                _   -> findColor cfg "white" White
printCC' _ ('|':'|':xs) = do
    putStrLn xs
    setSGR [Reset]
printCC' c (x:xs) = do
    putChar x
    printCC' c xs

findIntensity :: Config -> ColorIntensity
findIntensity cfg =
    tryFind cfg "c.intensity" Dull
        (\x ->
            case x of
                "vivid" -> Vivid
                _       -> Dull)

findColor :: Config -> String -> Color -> Color
findColor cfg name defColor =
    tryFind cfg ("c." ++ name) defColor
        (\x ->
            case x of
                "black"   -> Black
                "red"     -> Red
                "green"   -> Green
                "yellow"  -> Yellow
                "blue"    -> Blue
                "magenta" -> Magenta
                "cyan"    -> Cyan
                "white"   -> White
                _         -> defColor)
