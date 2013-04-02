module Control.Monad.Trans.Suspend where

import Control.Monad.Trans


data Suspend e a = Done a | Suspended (e -> Suspend e a)

nextEvent :: Suspend e e
nextEvent = Suspended Done

isDone :: Suspend e a -> Maybe a
isDone (Done x) = Just x
isDone _ = Nothing

sendEvent :: e -> Suspend e a -> Suspend e a
sendEvent _ (Done x) = Done x
sendEvent e (Suspended cc) = cc e


instance Functor (Suspend e) where
  fmap f (Done x) = Done (f x)
  fmap f (Suspended cc) = Suspended (fmap f . cc)

instance Monad (Suspend e) where
  return = Done
  Done x >>= f = f x
  Suspended cc >>= f = Suspended $ \e -> do x <- cc e
                                            f x
