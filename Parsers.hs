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

-- | The 'breakRepeat' function gets the position of end of the repeat statement
-- It takes a 'String' and two 'Int' as arguments and returns an 'Int' 
breakRepeat :: String -- ^ String to get position of corresponding ']' of the repeat
            -> Int    -- ^ Number of brackets not closed
            -> Int    -- ^ Current position
            -> Int    -- ^ Return position of ']'
breakRepeat string numBracks pos 
    
    | numBracks == 0     = pos
    | head string == ']' = breakRepeat (tail string) (numBracks-1) (pos+1)
    | head string == '[' = breakRepeat (tail string) (numBracks+1) (pos+1)
    | otherwise          = breakRepeat (tail string) (numBracks) (pos+1)

-- | The 'stringToCommands' function converts the input string into commands
-- It takes a 'String' as argument and returns a pair of 'String'
stringToCommands :: String            -- ^ String to get command from
                 -> (String, String)  -- ^ Return the command and the rest of the string as a pair
stringToCommands string

    | words string == []              = ("", "")
    | head (words string) == "clear"  = (head (words string), drop 6 string)
    | head (words string) /= "repeat" = (unwords (take 2 (words string)), unwords (drop 2 (words string)))
    | otherwise                       = (take ((breakRepeat str 1 0) + (fromJust('[' `elemIndex`  string)) +1) string, 
                                         drop ((breakRepeat str 1 0) + (fromJust('[' `elemIndex`  string)) +2) string)
    
    where str = drop (fromJust('[' `elemIndex` string) + 1) string
                          
-- | The 'splitString' function  breaks the string at first space
-- It takes a 'String' as argument and returns a pair of 'String'
splitString :: String           -- ^ String to split
            -> (String, String) -- ^ Pair of string formed after splitting the string
splitString string = splitAt (fromJust(' ' `elemIndex` string) + 1) string

-- | The 'ioToRender' function converts IO() to Render()
-- It takes an argument of type 'IO()' and returns an argument of type 'Render()'
ioToRender :: IO () -> Render ()
ioToRender _ = return ()

-- | The 'repeatCommand' function acts as a helper function to 'repeatRecurse'
-- It takes one argument of type 'DrawingArea', one argument of type 'String'. 
repeatCommand :: DrawingArea -- ^ The drawing area in which drawing takes place
              -> String      -- ^ String containing commands to be repeated
              -> Render () 
repeatCommand canvas commands = do

    let (command, restString) = stringToCommands commands 

    if command /= "" then
      updateCanvas canvas command
    
    else 
      return ()

    if restString == "" then
        return ()

    else repeatCommand canvas restString

-- | The 'repeatRecurse' function is used to execute commands containing the repeat statement
-- It takes one argument of type 'DrawingArea', one argument of type 'Int', one argument of type 'String'
repeatRecurse :: DrawingArea -- ^ The drawing area in which drawing takes place
              -> Int         -- ^ Number of times
              -> String      -- ^ String of the commands to be executed
              -> Render ()
repeatRecurse canvas times commands

    | times > 0 = do 
                    repeatCommand canvas commands
                    repeatRecurse canvas (times-1) commands
    
    | otherwise = return ()

-- | The 'updateCanvas' function is responsible for executing the command provided.
-- This function uses the functions declared in 'HelperFunctions.hs'.
updateCanvas :: DrawingArea -- ^ The drawing area in which drawing takes place
             -> String      -- ^ String of commands to be executed
             -> Render ()
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
