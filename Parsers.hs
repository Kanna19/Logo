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
    let distance = if (take 2 command) == "fd"
                    then read (drop 3 command) :: Double
                    else 0

    let angle = if (take 2 command) == "rt"
                    then read (drop 3 command) :: Double
                    else 0

    rotate (angle*pi/180)
    (w,h) <- getCurrentPoint
    --(w,h) <- (100, 100)

    lineTo w (h-distance)
    stroke
    markEnd w (h-distance)
    where markEnd x y = do
            setSourceRGB 0 1 0
            moveTo x y
            lineTo x y
            stroke
            setSourceRGB 1 0 0
            moveTo x y
            strokePreserve