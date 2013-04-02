module Main where

import System.Exit
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Fonts
import Graphics.UI.GLUT.Events
import Control.Monad.Trans.Suspend


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
                      renderText (-0.9, 0.5) 0.1 s
                      swapBuffers



glutMain :: IO ()
glutMain = do displayMessage "Are you sure? (y/n)"
              let loop (Char 'y') = do displayMessage "Full steam ahead, then!"
                                       afterDelay 1000 $ quitSequence
                  loop (Char 'n') = do displayMessage "Your loss."
                                       afterDelay 1000 $ quitSequence
                  loop _ = do displayMessage "Please answer with 'y' or 'n'."
                              afterDelay 1000 glutMain
              withKeypress loop


quitSequence :: IO ()
quitSequence = do displayMessage "Press ESC to quit."
                  let loop (Char '\x1b' {-esc-}) = delayedExit
                      loop _ = withKeypress loop
                  withKeypress loop

delayedExit :: IO ()
delayedExit = do displayMessage "Bye!"
                 afterDelay 1000 $ exitSuccess


testTimeouts :: Int -> IO ()
testTimeouts 0 = quitSequence
testTimeouts n = do displayMessage $ show n
                    afterDelay 1000 $ testTimeouts $ n-1


echoKeys :: Suspend Char (IO ())
echoKeys = helper "" where
  helper cs = do c <- nextEvent
                 case c of
                   '\x0d' {-enter-} -> return $ putStrLn $ reverse cs
                   _ -> do let act1 = putStrLn $ c:""
                           act2 <- helper $ c:cs
                           return $ do act1
                                       act2

keydownResume :: Suspend Char (IO ()) -> IO ()
keydownResume sx = case isDone sx of
                     Just act -> do act
                                    exitSuccess
                     Nothing -> withKeypress resume
                   where
  resume (Char c) = keydownResume (sendEvent c sx)
  resume _        = withKeypress resume

main = do getArgsAndInitialize
          createWindow "glut-events demo"
          
          withGlutMain $ do displayMessage "type then press ENTER."
                            keydownResume echoKeys
