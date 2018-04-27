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

    if (take 2 command) == "fd" then
        moveForward argument

    else if (take 2 command) == "rt" then
        turnRight argument

    else if (take 2 command) == "lt" then
        turnLeft argument

    else
        return ()

        where argument = read(drop 3 command) :: Double


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