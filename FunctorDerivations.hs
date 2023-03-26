module FunctorDerivations where

import Control.Applicative

-- Derive the `Functor` and `Applicative` instances for Maybe',
-- using ONLY `>>=` and `return`
data Maybe' a = Just' a | Nothing'

instance Monad Maybe' where
  Nothing'  >>= _ = Nothing'
  (Just' a) >>= f = f a

  return = Just'

instance Applicative Maybe' where
  pure = return

  mf <*> ma =
    mf >>= \f ->
      ma >>= \a ->
        pure $ f a

  liftA2 f ma mb =
    ma >>= \a ->
      mb >>= \b ->
        pure $ f a b

instance Functor Maybe' where
  fmap f m =
    m >>= \a -> pure $ f a


-- Derive the `Functor` instance for Maybe'',
-- using ONLY `pure` and `<*>`
data Maybe'' a = Just'' a | Nothing''

instance Applicative Maybe'' where
  pure = Just''
  
  (Just'' f) <*> (Just'' a) = Just'' $ f a
  _          <*> _          = Nothing''

instance Functor Maybe'' where
  fmap f m = pure f <*> m
