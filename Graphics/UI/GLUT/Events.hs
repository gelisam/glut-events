module Graphics.UI.GLUT.Events where

import Control.Monad
import Graphics.UI.GLUT


data EventHook e = EventHook { registerCallback :: (e -> IO ()) -> IO ()
                             , unregisterCallback :: IO ()
                             }

withNextEvent :: EventHook e -> (e -> IO ()) -> IO ()
withNextEvent h cc = registerCallback h handler where
  handler e = do unregisterCallback h
                 cc e

untilEvent :: EventHook () -> IO () -> IO ()
untilEvent h = withNextEvent h . const


subevent :: EventHook e -> (e -> Maybe a) -> EventHook a
subevent h p = EventHook register unregister where
  maybeForward cc e = case p e of
                        Just x -> cc x
                        Nothing -> return ()
  register cc = registerCallback h $ maybeForward cc
  unregister = unregisterCallback h

exactEvent :: Eq e => EventHook e -> e -> EventHook ()
exactEvent h e = subevent h isEqual where
  isEqual = guard . (== e)


displayHook :: EventHook ()
displayHook = EventHook register unregister where
  register f = displayCallback $= f ()
  unregister = displayCallback $= return ()

withGlutMain :: IO () -> IO ()
withGlutMain glutMain = do untilEvent displayHook glutMain
                           mainLoop


keyboardMouseHook :: EventHook (Key, KeyState, Modifiers, Position)
keyboardMouseHook = EventHook register unregister where
  curry4 f w x y z = f (w, x, y, z)
  register f = keyboardMouseCallback $= Just (curry4 f)
  unregister = keyboardMouseCallback $= Nothing

keydownHook :: EventHook Key
keydownHook = subevent keyboardMouseHook isDown where
  isDown (k, Down, _, _) = Just k
  isDown _ = Nothing

keyupHook :: EventHook Key
keyupHook = subevent keyboardMouseHook isUp where
  isUp (k, Up, _, _) = Just k
  isUp _ = Nothing

withKeydown :: (Key -> IO ()) -> IO ()
withKeydown = withNextEvent keydownHook

withKeyup :: (Key -> IO ()) -> IO ()
withKeyup = withNextEvent keyupHook

withKeypress :: (Key -> IO ()) -> IO ()
withKeypress cc = withKeydown untilReleased where
  untilReleased k = untilEvent (exactEvent keyupHook k) $ cc k


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
