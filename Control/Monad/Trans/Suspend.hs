module Control.Monad.Trans.Suspend where

import Control.Monad.Trans
import Control.Monad.Identity


-- As is often the case, the MonadTransformer version is much harder to understand
-- than the plain version. For your convenience, here is how (Suspend e) would be
-- implemented if it wasn't defined as (SuspendT e Identity).
-- 
--    data Suspend e a = Done a | Suspended (e -> Suspend e a)
--    
--    nextEvent :: Suspend e e
--    nextEvent = Suspended Done
--    
--    
--    instance Functor (Suspend e) where
--      fmap f (Done x) = Done (f x)
--      fmap f (Suspended cc) = Suspended (fmap f . cc)
--    
--    instance Monad (Suspend e) where
--      return = Done
--      Done x >>= f = f x
--      Suspended cc >>= f = Suspended $ \e -> do x <- cc e
--                                                f x


-- From the inside, a suspendable computation of type (Suspend e a) is similar to a
-- (Reader e a), except you get a new value every time you read it (using nextEvent).

nextEvent :: Monad m => SuspendT e m e
nextEvent = SuspendT $ return $ Suspended $ SuspendT . return . Done


-- From the outside, a suspendable computation consumes an unspecified number of events
-- of type e, then halts with a value of type a. The difference between this and a
-- function of type ([e] -> a) is that the caller gets to know when to stop generating
-- events, so it could generate them using side effects.

runSuspend :: Suspend e a -> Suspend1 e (SuspendT e Identity) a
runSuspend = runIdentity . runSuspendT

isDone :: Suspend e a -> Maybe a
isDone sx = case runSuspend sx of
              Done x -> Just x
              _ -> Nothing

sendEvent :: e -> Suspend e a -> Suspend e a
sendEvent e sx = case runSuspend sx of
                   Done x -> return x
                   Suspended cc -> cc e

sendEvents :: [e] -> Suspend e a -> a
sendEvents (e:es) sx = case runSuspend sx of
                         Done x -> x
                         Suspended cc -> sendEvents es (cc e)

generateEvents :: Monad m => Suspend e a -> m e -> m a
generateEvents sx gen_e = case isDone sx of
                            Just x -> return x
                            Nothing -> do
                              e <- gen_e
                              generateEvents (sendEvent e sx) gen_e


-- The monad transformer version is more useful, as the suspendable computation can
-- cause side-effects in between its event requests, possibly affecting which events
-- it will receive.

data Suspend1 e m a = Done a | Suspended (e -> m a)
data SuspendT e m a = SuspendT { runSuspendT :: m (Suspend1 e (SuspendT e m) a) }
type Suspend e a = SuspendT e Identity a

instance MonadTrans (SuspendT e) where
  lift mx = SuspendT $ do x <- mx
                          return $ Done x

instance Functor m => Functor (Suspend1 e m) where
  fmap f (Done a) = Done $ f a
  fmap f (Suspended cc) = Suspended $ fmap f . cc

instance Functor m => Functor (SuspendT e m) where
  fmap f tx = SuspendT $ fmap (fmap f) $ runSuspendT tx

instance Monad m => Monad (SuspendT e m) where
  return = SuspendT . return . Done
  tx >>= f = SuspendT $ do
               sx <- runSuspendT tx
               case sx of
                 Done x       -> runSuspendT $ f x
                 Suspended cc -> return $ Suspended $ \e -> cc e >>= f
