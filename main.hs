import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Data.List
import Data.Typeable
import Text.Read

main :: IO ()
main = do
    -- Start GUI loop
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

    btn <- buttonNew
    set btn [ buttonLabel := "Enter"]
    btn `on` buttonActivated $ do
        inputBuffer <- textViewGetBuffer inputPart
        iter <- textBufferGetStartIter displayBuffer
        start <- textBufferGetStartIter inputBuffer
        end <- textBufferGetEndIter inputBuffer
        tempText <- textBufferGetText inputBuffer start end False
        canvas `on` draw $ updateCanvas canvas tempText
        widgetQueueDraw canvas
        textBufferInsert displayBuffer iter (tempText ++ "\n")

        return ()

    displayScroll <- scrolledWindowNew Nothing Nothing
    containerAdd displayScroll displayPart
    widgetSetSizeRequest displayScroll 800 100
    scrolledWindowSetShadowType displayScroll ShadowOut

    inputScroll <- scrolledWindowNew Nothing Nothing
    containerAdd inputScroll inputPart
    widgetSetSizeRequest inputScroll 800 100
    scrolledWindowSetShadowType inputScroll ShadowIn

    vbox <- vBoxNew False 0

    boxPackStart vbox canvas PackNatural 0
    boxPackStart vbox displayScroll PackNatural 0
    boxPackStart vbox inputScroll PackNatural 0
    boxPackStart vbox btn PackNatural 0

    containerAdd window vbox

    window `on` deleteEvent $ do -- handler to run on window destruction
       liftIO mainQuit
       return False
    widgetShowAll window
    mainGUI

stringToMaybeString :: String -> Maybe String
stringToMaybeString x
 | True = Just x
 | otherwise = Nothing

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

centreTurtle :: DrawingArea -> Render ()
centreTurtle canvas = do
    width'  <- liftIO $ widgetGetAllocatedWidth  canvas
    height' <- liftIO $ widgetGetAllocatedHeight canvas
    let width  = realToFrac width'
        height = realToFrac height'

    setSourceRGB 1 0 0
    setLineWidth 4
    setLineCap LineCapRound
    setLineJoin LineJoinRound

    moveTo (width/2) (height/2)
    lineTo (width/2) (height/2)
    strokePreserve

renderToIO :: Render () -> IO ()
renderToIO world = return ()
