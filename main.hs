import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo
import Data.List
import Data.Typeable

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

    btn <- buttonNew
    set btn [ buttonLabel := "Enter"]
    btn `on` buttonActivated $ do
        inputBuffer <- textViewGetBuffer inputPart
        iter <- textBufferGetStartIter displayBuffer
        start <- textBufferGetStartIter inputBuffer
        end <- textBufferGetEndIter inputBuffer
        tempText <- textBufferGetText inputBuffer start end False
        textBufferInsert displayBuffer iter (tempText ++ "\n")

        return ()

    canvas <- drawingAreaNew
    widgetSetSizeRequest canvas 800 300

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
