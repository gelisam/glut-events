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
