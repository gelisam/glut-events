module Main where

import System.Exit
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Fonts


scale_uniformly :: GLdouble -> IO ()
scale_uniformly f = scale f f f

drawWindow :: IO ()
drawWindow = do clear [ColorBuffer]
                renderString Helvetica18 "Press ESC to quit."
                swapBuffers

handleEvents :: Key -> KeyState -> Modifiers -> Position -> IO ()
handleEvents (Char '\x1b' {-esc-}) _ _ _ = exitSuccess
handleEvents _ _ _ _ = return ()


main = do getArgsAndInitialize
          createWindow "99 bottles"
          
          keyboardMouseCallback $= Just handleEvents
          displayCallback $= drawWindow
          
          mainLoop
