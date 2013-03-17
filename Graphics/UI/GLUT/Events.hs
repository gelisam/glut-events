module Graphics.UI.GLUT.Events where

import Graphics.UI.GLUT


withGlutMain :: IO () -> IO ()
withGlutMain glutMain = do displayCallback $= do reset
                                                 glutMain
                           mainLoop
                        where
  reset = displayCallback $= noop
  noop = return ()


withKeyboardMouseEvent :: (Key -> KeyState -> Modifiers -> Position -> IO ()) -> IO ()
withKeyboardMouseEvent cc = keyboardMouseCallback $= Just handleAndContinue where
  handleAndContinue k s m p = do reset
                                 cc k s m p
  reset = keyboardMouseCallback $= Nothing

withKeydown :: (Key -> IO ()) -> IO ()
withKeydown cc = withKeyboardMouseEvent handleEvent where
  handleEvent k Down _ _ = cc k
  handleEvent _ _ _ _ = withKeydown cc

withKeyup :: (Key -> IO ()) -> IO ()
withKeyup cc = withKeyboardMouseEvent handleEvent where
  handleEvent k Up _ _ = cc k
  handleEvent _ _ _ _ = withKeyup cc

withKeypress :: (Key -> IO ()) -> IO ()
withKeypress cc = withKeydown untilReleased where
  untilReleased k = withKeyup $ \k' -> if k == k'
                                         then cc k
                                         else untilReleased k


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
