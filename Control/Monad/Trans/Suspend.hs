module Control.Monad.Trans.Suspend where

import Control.Monad.Trans


data Suspend e a = Done a | Suspended (e -> Suspend e a)

nextEvent :: Suspend e e
nextEvent = Suspended Done


instance Functor (Suspend e) where
  fmap f (Done x) = Done (f x)
  fmap f (Suspended cc) = Suspended (fmap f . cc)

instance Monad (Suspend e) where
  return = Done
  Done x >>= f = f x
  Suspended cc >>= f = Suspended $ \e -> do x <- cc e
                                            f x
