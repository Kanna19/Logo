module Parsers where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Data.List
import Data.Typeable
import Text.Read
import Data.Maybe

stringToCommands :: String -> (String, String)
stringToCommands string = if ' ' `elemIndex` (drop 3 string) /= Nothing then
                          splitAt (fromJust(' ' `elemIndex` (drop 3 string)) + 4) string
                          else (string, "")

stringToCommandss :: String -> (String, String)
stringToCommandss string = splitAt (fromJust(' ' `elemIndex` string) + 1) string

ioToRender :: IO () -> Render ()
ioToRender _ = return ()
repeatCommand :: DrawingArea -> String -> Render ()
repeatCommand canvas commands = do
    let (command, restString) = stringToCommands commands 

    updateCanvas canvas command

    if restString == "" then
        return ()

    else repeatCommand canvas restString

repeatCommands :: DrawingArea -> Int -> String -> Render ()
repeatCommands canvas times commands = do
    if times > 0 then do
        repeatCommand canvas commands
        repeatCommands canvas (times-1) commands
    else return ()



updateCanvas :: DrawingArea -> String -> Render ()
updateCanvas canvas command = do

    ioToRender (putStrLn "Bom!")

    if (take 2 command) == "fd" then
        moveForward argument

    else if (take 2 command) == "rt" then
        turnRight argument

    else if (take 2 command) == "lt" then
        turnLeft argument

    else if (take 6 command) == "repeat" then
        repeatCommands canvas ((read repArg) :: Int) (init (tail repCom))

    else
        return ()

        where argument = read(drop 3 command) :: Double
              (repArg,repCom) = stringToCommandss (drop 7 command)


moveForward :: Double -> Render ()
moveForward distance = do

    (w, h) <- getCurrentPoint

    lineTo w (h - distance)
    stroke

    markEnd w (h - distance)
    where markEnd x y = do
            setSourceRGB 0 1 0
            moveTo x y
            lineTo x y
            stroke
            setSourceRGB 1 0 0
            moveTo x y
            strokePreserve

turnRight :: Double -> Render ()
turnRight angle = do

    rotate (angle *pi / 180)
    (w, h) <- getCurrentPoint

    markEnd w h
    where markEnd x y = do
            setSourceRGB 0 1 0
            moveTo x y
            lineTo x y
            stroke
            setSourceRGB 1 0 0
            moveTo x y
            strokePreserve

turnLeft :: Double -> Render ()
turnLeft angle = do

    turnRight (-1 *angle)