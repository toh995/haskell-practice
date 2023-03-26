module Practice where

import Control.Monad
import Control.Monad.State.Lazy (MonadTrans, lift, MonadState, get, put)

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  (Identity a) >>= k = k a

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

-- Implement a monad transformer IdentityT, analogous to Identity but
-- wrapping values of type m a rather than a.
-- Write at least its Monad and MonadTrans instances.
newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance MonadTrans IdentityT where
  lift :: Monad m => m a -> IdentityT m a
  lift = IdentityT

instance Monad m => Monad (IdentityT m) where
  (>>=) (IdentityT ma) f =
    IdentityT $ ma >>= runIdentityT . f

instance Monad m => Applicative (IdentityT m) where
  pure = IdentityT . pure

  (<*>) (IdentityT mf) (IdentityT ma) =
    IdentityT $ mf <*> ma

instance Monad m => Functor (IdentityT m) where
  fmap f (IdentityT ma) =
    IdentityT $ f <$> ma

-- Implement state :: MonadState s m => (s -> (a, s)) -> m a in terms of get and put.
state :: MonadState s m => (s -> (a, s)) -> m a
state f = do
  s0 <- get
  let (a, s1) = f s0
  put s1
  pure a

-- Are MaybeT (State s) and StateT s Maybe equivalent?
-- (Hint: one approach is comparing what the run...T unwrappers produce in each case.)

-- NO!

-- MaybeT (State s) = MaybeT (StateT s Identity)
--                  -> StateT s Identity (Maybe a)
--                  -> s -> Identity (Maybe a, s)

-- StateT s Maybe -> s -> Maybe (a, s)
