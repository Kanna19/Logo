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

breakRepeat :: String -> Int -> Int -> Int
breakRepeat string numBracks pos 
    | numBracks == 0 = pos
    | head string == ']' = breakRepeat (tail string) (numBracks-1) (pos+1)
    | head string == '[' = breakRepeat (tail string) (numBracks+1) (pos+1)
    | otherwise = breakRepeat (tail string) (numBracks) (pos+1)

stringToCommands :: String -> (String, String)
stringToCommands string = if head (words string) /= "repeat" then 
                            (unwords (take 2 (words string)), unwords (drop 2 (words string)))

                          else
                            (take ((breakRepeat str 1 0) + (fromJust('[' `elemIndex`  string)) +1) string, 
                                drop ((breakRepeat str 1 0) + (fromJust('[' `elemIndex`  string)) +2) string)
                        where 
                            str = drop (fromJust('[' `elemIndex`  string) +1) string
                          
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

    case firstWord of "fd"     -> moveForward argument
                      "rt"     -> turnRight argument
                      "lt"     -> turnLeft argument
                      "bk"     -> moveBackward argument
                      "tree"   -> tree argument
                      "clear"  -> clearScreen
                      "repeat" -> repeatCommands canvas ((read repArg) :: Int) (init (tail repCom))
                      _        -> return ()

    where firstWord        = head (words command)
          argument         = read (head (tail((words command)))) :: Double
          (repArg, repCom) = stringToCommandss (drop 7 command)

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
    markEnd 400 150
    where markEnd x y = do
            setSourceRGB 0 1 0
            moveTo x y
            lineTo x y
            stroke
            setSourceRGB 1 0 0
            moveTo x y
            strokePreserve