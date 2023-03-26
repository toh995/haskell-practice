module State where

newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = State

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f stateA = 
    State $
      \s0 ->
        let (a, s1) = runState stateA s0
         in (f a, s1)

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State (x,)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (<*>) stateF stateA =
    State $
      \s0 ->
        let (f, s1) = runState stateF s0
            (a, s2) = runState stateA s1
         in (f a, s2)

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (>>=) stateA g =
    State $
      \s0 ->
        let (a, s1) = runState stateA s0
            stateB = g a
         in runState stateB s1

evalState :: State s a -> s -> a
evalState p s = fst (runState p s)

execState :: State s a -> s -> s
execState p s = snd (runState p s)

put :: s -> State s ()
put newState = state $ const ((), newState)

get :: State s s
get = state $ \s -> (s, s)

-- Besides put and get, there are also
-- modify :: (s -> s) -> State s ()
-- which modifies the current state using a function, and
-- gets :: (s -> a) -> State s a
-- which produces a modified copy of the state while leaving the state itself unchanged.
-- Write implementations for them.
modify :: (s -> s) -> State s ()
modify f = get >>= \s0 -> put (f s0)
-- modify f = do
--   s0 <- get
--   put (f s0)

gets :: (s -> a) -> State s a
gets f = f <$> get
-- gets f = get >>= \s0 -> pure (f s0)
-- gets f = do
--   s0 <- get
--   pure (f s0)
