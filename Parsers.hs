module Parsers where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Data.List
import Data.Typeable
import Text.Read

updateCanvas :: DrawingArea -> String -> Render ()
updateCanvas canvas command = do

    case firstTwoLetters of "fd" -> moveForward argument_2
                            "rt" -> turnRight argument_2
                            "lt" -> turnLeft argument_2
                            "bk" -> moveBackward argument_2
                            _    -> return ()

    where firstTwoLetters  = take 2 command
          firstFourLetters = take 4 command
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