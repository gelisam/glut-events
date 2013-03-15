module Main where

import System.Exit
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Fonts


scale_uniformly :: GLdouble -> IO ()
scale_uniformly f = scale f f f

renderText :: (GLdouble, GLdouble) -> GLdouble -> String -> IO ()
renderText (x, y) h s = preservingMatrix $ do translate (Vector3 x y 0.0)
                                              scale_uniformly (h / 100.0)
                                              renderString Roman s

-- the default GLUT window coordinates go from (-1, 1) at the top-left
-- to (1, -1) at the bottom right, so this places the message towards
-- the top left, with a font size of 5% of the window.
displayMessage :: String -> IO ()
displayMessage s = do clear [ColorBuffer]
                      renderText (-0.8, 0.5) 0.1 s
                      swapBuffers


glutMain :: IO ()
glutMain = quitSequence


quitSequence :: IO ()
quitSequence = do displayMessage "Press ESC to quit."
                  keyboardMouseCallback $= Just waitForEsc

waitForEsc :: Key -> KeyState -> Modifiers -> Position -> IO ()
waitForEsc (Char '\x1b' {-esc-}) _ _ _ = delayedExit
waitForEsc _ _ _ _ = return ()

delayedExit :: IO ()
delayedExit = do displayMessage "Bye!"
                 addTimerCallback 1000 $ exitSuccess


main = do getArgsAndInitialize
          createWindow "glut-events demo"
          
          displayCallback $= glutMain
          
          mainLoop
