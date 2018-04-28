import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Data.List
import Data.Typeable
import Text.Read
import Parsers
import System.IO
import HelperFunctions

-- | The function 'main' handles IO
main :: IO ()
main = do

    -- Allocate required resources for GTK+ to work
    -- void is used as no command line arguments are needed
    void initGUI

    -- Create main window
    window <- windowNew
    set window [ windowTitle         := "Logo"
               , windowResizable     := False ]

    -- Create text view to display history
    displayPart <- textViewNew

    -- Make display part uneditable
    textViewSetEditable displayPart False

    -- Get text buffer from display part
    displayBuffer <- textViewGetBuffer displayPart

    -- Create text view to take input
    inputPart <- textViewNew
 
    -- Create new Canvas
    canvas <- drawingAreaNews

    -- Set the size of the canvas
    widgetSetSizeRequest canvas 800 300

    -- Render functions to execute on drawing canvas
    canvas `on` draw $ centreTurtle canvas
    canvas `on` draw $ clearScreen
    canvas `on` draw $ drawTurtle

    -- Make commands.txt empty
    writeFile "commands.txt" ""

    -- Open file in read write mode
    handle <- openFile "commands.txt" ReadWriteMode
    
    -- Initial position of the handle
    pos <- hGetPosn handle

    -- Create enter button
    enterButton <- buttonNew

    -- Set the text of enter button
    set enterButton [ buttonLabel := "Enter"]

    -- Actions to be done when enter button is clicked
    enterButton `on` buttonActivated $ do

        -- Set current position to the start of the file
        curPos <- hGetPosn handle
        hSetPosn pos

        -- Check if EOF is detected
        isEnd <- hIsEOF handle

        commands <- if isEnd then
                        return ""

                    else hGetLine handle

        canvas `on` draw $ clearScreen
        canvas `on` draw $ updateCanvas canvas ("repeat 1 [" ++ commands ++ "]")

        hSetPosn curPos
        
        -- Get text buffer from input part
        inputBuffer <- textViewGetBuffer inputPart

        -- Iterator to iterate through text buffer
        iter <- textBufferGetStartIter displayBuffer

        -- Start and end iterators
        start <- textBufferGetStartIter inputBuffer
        end <- textBufferGetEndIter inputBuffer
        
        -- Commands to be executed
        tempText <- textBufferGetText inputBuffer start end False
        
        -- Send the entered commands to updateCanvas
        canvas `on` draw $ updateCanvas canvas (("repeat 1 [" ++ tempText) ++ "]")
        
        -- Draw turtle
        canvas `on` draw $ drawTurtle

        -- Request redrawing of canvas
        widgetQueueDraw canvas
        
        -- Write the input commands in commands.txt
        hPutStr handle (tempText ++ " ")
        textBufferInsert displayBuffer iter (tempText ++ "\n")

        return ()

    -- Create enter button
    saveButton <- buttonNew

    -- Set the text of the save button
    set saveButton [ buttonLabel := "Save"]

    -- Create new surface to convert to image
    srf <- createImageSurface FormatARGB32 800 600
    
    -- Actions to be done when save is clicked
    saveButton `on` buttonActivated $ do
        
        -- Set the position of the handle to the start of the file
        hSetPosn pos

        -- Get the commands
        commands <- hGetContents handle
        renderWith srf (updateCanvas canvas ("repeat 1 [clear " ++ commands ++ "]"))
        surfaceWriteToPNG srf "Picture.png"
        
        -- Exit after saving
        liftIO mainQuit
        return ()

    -- Functions arranging the layout of the program
    displayScroll <- scrolledWindowNew Nothing Nothing
    containerAdd displayScroll displayPart
    widgetSetSizeRequest displayScroll 800 100
    scrolledWindowSetShadowType displayScroll ShadowOut

    inputScroll <- scrolledWindowNew Nothing Nothing
    containerAdd inputScroll inputPart
    widgetSetSizeRequest inputScroll 800 100
    scrolledWindowSetShadowType inputScroll ShadowIn

    hbox <- hBoxNew False 0
    boxPackStart hbox enterButton PackNatural 0
    boxPackStart hbox saveButton PackNatural 0

    vbox <- vBoxNew False 0

    boxPackStart vbox canvas PackNatural 0
    boxPackStart vbox displayScroll PackNatural 0
    boxPackStart vbox inputScroll PackNatural 0
    boxPackStart vbox hbox PackNatural 0

    containerAdd window vbox

    window `on` deleteEvent $ do -- handler to run on window destruction
       
       liftIO mainQuit
       return False

    -- Make the window and all its children visible
    widgetShowAll window

    -- Start the main loop
    -- This loop listens to events such as button clicks, mouse movement etc
    mainGUI