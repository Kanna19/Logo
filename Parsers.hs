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
import HelperFunctions

breakRepeat :: String -> Int -> Int -> Int
breakRepeat string numBracks pos 
    
    | numBracks == 0     = pos
    | head string == ']' = breakRepeat (tail string) (numBracks-1) (pos+1)
    | head string == '[' = breakRepeat (tail string) (numBracks+1) (pos+1)
    | otherwise          = breakRepeat (tail string) (numBracks) (pos+1)

stringToCommands :: String -> (String, String)
stringToCommands string

    | words string == []              = ("", "")
    | head (words string) == "clear"  = (head (words string), drop 6 string)
    | head (words string) /= "repeat" = (unwords (take 2 (words string)), unwords (drop 2 (words string)))
    | otherwise                       = (take ((breakRepeat str 1 0) + (fromJust('[' `elemIndex`  string)) +1) string, 
                                         drop ((breakRepeat str 1 0) + (fromJust('[' `elemIndex`  string)) +2) string)
    
    where str = drop (fromJust('[' `elemIndex` string) + 1) string
                          
splitString :: String -> (String, String)
splitString string = splitAt (fromJust(' ' `elemIndex` string) + 1) string

ioToRender :: IO () -> Render ()
ioToRender _ = return ()

repeatCommand :: DrawingArea -> String -> Render ()
repeatCommand canvas commands = do

    let (command, restString) = stringToCommands commands 

    if command /= "" then
      updateCanvas canvas command
    
    else 
      return ()

    if restString == "" then
        return ()

    else repeatCommand canvas restString

repeatRecurse :: DrawingArea -> Int -> String -> Render ()
repeatRecurse canvas times commands

    | times > 0 = do 
                    repeatCommand canvas commands
                    repeatRecurse canvas (times-1) commands
    
    | otherwise = return ()

updateCanvas :: DrawingArea -> String -> Render ()
updateCanvas canvas command = do

    case firstWord of "fd"     -> moveForward argument
                      "rt"     -> turnRight argument
                      "lt"     -> turnLeft argument
                      "bk"     -> moveBackward argument
                      "tree"   -> tree argument
                      "clear"  -> clearScreen
                      "repeat" -> repeatRecurse canvas ((read repArg) :: Int) (init (tail repCom))
                      "exit"   -> liftIO mainQuit
                      _        -> return ()

    where firstWord        = head (words command)
          argument         = read (head (tail((words command)))) :: Double
          (repArg, repCom) = splitString (drop 7 command)
