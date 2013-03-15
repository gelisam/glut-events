module Graphics.UI.GLUT.Events where

import Graphics.UI.GLUT


withGlutMain :: IO () -> IO ()
withGlutMain glutMain = do displayCallback $= do reset
                                                 glutMain
                           mainLoop
                        where
  reset = displayCallback $= noop
  noop = return ()

withKeypress :: (Key -> IO ()) -> IO ()
withKeypress cc = keyboardMouseCallback $= Just handleAndContinue where
  handleAndContinue k _ _ _ = do reset
                                 cc k
  reset = keyboardMouseCallback $= Nothing

-- in milliseconds.
-- please use this instead of addTimerCallback, as the latter doesn't
-- work very well with delays larger than 900 milliseconds.
afterDelay :: Timeout -> IO () -> IO ()
afterDelay d cc | d > 500 = addTimerCallback 500 $ afterDelay (d-500) cc
afterDelay d cc | otherwise = addTimerCallback d cc
