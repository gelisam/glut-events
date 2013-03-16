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
-- please use this instead of addTimerCallback,
-- as the latter often triggers too early.
afterDelay :: Timeout -> IO () -> IO ()
afterDelay d cc = do t0 <- get elapsedTime
                     waitUntil (t0 + d)
                  where
  waitUntil t_goal = do t <- get elapsedTime
                        if t < t_goal
                           then addTimerCallback (t_goal - t) $
                                waitUntil t_goal
                           else cc
