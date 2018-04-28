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

main :: IO ()
main = do

    -- Allocate required resources for GTK+ to work
    -- void is used as no command line arguments are needed
    void initGUI

    -- Create main window
    window <- windowNew
    set window [ windowTitle         := "Logo"
               , windowResizable     := False ]

    displayPart <- textViewNew

    textViewSetEditable displayPart False
    displayBuffer <- textViewGetBuffer displayPart

    inputPart <- textViewNew
    --textViewSetBorderWindowSize inputPart  TextWindowTop 200

    canvas <- drawingAreaNew
    widgetSetSizeRequest canvas 800 300

    canvas `on` draw $ centreTurtle canvas
    canvas `on` draw $ clearScreen

    writeFile "commands.txt" ""
    handle <- openFile "commands.txt" ReadWriteMode
    pos <- hGetPosn handle

    enterButton <- buttonNew
    set enterButton [ buttonLabel := "Enter"]

    enterButton `on` buttonActivated $ do
        
        inputBuffer <- textViewGetBuffer inputPart
        iter <- textBufferGetStartIter displayBuffer
        start <- textBufferGetStartIter inputBuffer
        end <- textBufferGetEndIter inputBuffer
        tempText <- textBufferGetText inputBuffer start end False
        
        canvas `on` draw $ updateCanvas canvas (("repeat 1 [" ++ tempText) ++ "]")
        widgetQueueDraw canvas
        
        hPutStr handle (tempText ++ " ")
        textBufferInsert displayBuffer iter (tempText ++ "\n")

        return ()

    saveButton <- buttonNew
    set saveButton [ buttonLabel := "Save"]

    srf <- createImageSurface FormatARGB32 800 600
    
    saveButton `on` buttonActivated $ do
    
        hSetPosn pos
        commands <- hGetContents handle
        renderWith srf (updateCanvas canvas ("repeat 1 [clear " ++ commands ++ "]"))
        surfaceWriteToPNG srf "Picture.png"
        liftIO mainQuit
        return ()

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

centreTurtle :: DrawingArea -> Render ()
centreTurtle canvas = do
    
    width'  <- liftIO $ widgetGetAllocatedWidth  canvas
    height' <- liftIO $ widgetGetAllocatedHeight canvas
    
    let width  = realToFrac width'
        height = realToFrac height'

    setSourceRGB 1 0 0
    setLineWidth 1
    setLineCap LineCapRound
    setLineJoin LineJoinRound

    moveTo (width/2) (height/2)
    lineTo (width/2) (height/2)
    strokePreserve
