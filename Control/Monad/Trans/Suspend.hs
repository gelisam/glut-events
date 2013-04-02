module Control.Monad.Trans.Suspend where

import Control.Monad.Trans


-- From the inside, a suspendable computation of type (Suspend e a) is similar to a
-- (Reader e a), except you get a new value every time you read it (using nextEvent).

data Suspend e a = Done a | Suspended (e -> Suspend e a)

nextEvent :: Suspend e e
nextEvent = Suspended Done

-- From the outside, a suspendable computation consumes an unspecified number of events
-- of type e, then halts with a value of type a. The difference between this and a
-- function of type ([e] -> a) is that the caller gets to know when to stop generating
-- events, so it could generate them using side effects.

isDone :: Suspend e a -> Maybe a
isDone (Done x) = Just x
isDone _ = Nothing

sendEvent :: e -> Suspend e a -> Suspend e a
sendEvent _ (Done x) = Done x
sendEvent e (Suspended cc) = cc e

sendEvents :: Monad m => Suspend e a -> m e -> m a
sendEvents sx gen_e = case isDone sx of
                        Just x -> return x
                        Nothing -> do e <- gen_e
                                      sendEvents (sendEvent e sx) gen_e


instance Functor (Suspend e) where
  fmap f (Done x) = Done (f x)
  fmap f (Suspended cc) = Suspended (fmap f . cc)

instance Monad (Suspend e) where
  return = Done
  Done x >>= f = f x
  Suspended cc >>= f = Suspended $ \e -> do x <- cc e
                                            f x
