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

    case firstTwoLetters of "fd" -> moveForward argument_2
                            "rt" -> turnRight argument_2
                            "lt" -> turnLeft argument_2
                            "bk" -> moveBackward argument_2
                            "cl" -> clearScreen
                            _    -> return ()

    case firstFourLetters of "tree" -> tree argument_4
                             _      -> return ()

    case firstSixLetters of "repeat" -> repeatCommands canvas ((read repArg) :: Int) (init (tail repCom))
                            _        -> return ()
                            
    where firstTwoLetters  = take 2 command
          firstFourLetters = take 4 command
          firstSixLetters  = take 6 command
          (repArg, repCom) = stringToCommandss (drop 7 command)
          argument_2       = read(drop 3 command) :: Double
          argument_4       = read(drop 5 command) :: Double

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

moveBackward :: Double -> Render ()
moveBackward distance = moveForward (-1 * distance)

turnRight :: Double -> Render ()
turnRight angle = do

    rotate (angle * pi / 180)
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
turnLeft angle = turnRight (-1 * angle)

tree :: Double -> Render ()
tree size
    | size < 5  = do
                  moveForward size 
                  moveBackward size
                  return ()
 
    | otherwise = do
                  moveForward (size / 3)  
                  turnLeft 30
                  tree (2 * size / 3)
                  turnRight 30
                  moveForward (size / 6)
                  turnRight 25
                  tree (size / 2)
                  turnLeft 25
                  moveForward (size / 3)
                  turnRight 25
                  tree (size / 2)
                  turnLeft 25
                  moveForward (size / 6)
                  moveBackward size
                  return ()

clearScreen :: Render ()
clearScreen = do
    setSourceRGB 1 1 1
    paint
    strokePreserve

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