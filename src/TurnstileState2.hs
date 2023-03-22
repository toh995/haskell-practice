module TurnstileState2 where

import Control.Monad
import Data.Functor

import State
import Turnstile

data TurnstileInput = Coin | Push
  deriving (Eq, Show)
  
turnS :: TurnstileInput -> State TurnstileState TurnstileOutput
turnS = State . turn where
  turn Coin _        = (Thank, Unlocked)
  turn Push Unlocked = (Open,  Locked)
  turn Push Locked   = (Tut,   Locked)

coinS :: State TurnstileState TurnstileOutput
coinS = turnS Coin

pushS :: State TurnstileState TurnstileOutput
pushS = turnS Push

getsThroughS :: TurnstileInput -> State TurnstileState Bool
getsThroughS input = do
  output <- turnS input
  return $ output == Open

countOpens :: [TurnstileInput] -> State TurnstileState Int
countOpens = foldM incIfOpens 0 where
  incIfOpens :: Int -> TurnstileInput -> State TurnstileState Int
  incIfOpens n i = do
    g <- getsThroughS i
    if g then return (n+1) else return n

-- Modify regularPersonS, distractedPersonS and hastyPersonS to use turnS and mapM.
regularPersonS, distractedPersonS, hastyPersonS :: State TurnstileState [TurnstileOutput]

regularPersonS = mapM turnS [Coin, Push]

distractedPersonS = mapM turnS [Coin]

hastyPersonS =
  turnS Push >>= \a1 ->
    if a1 == Open
    then
      pure [a1]
    else
      (a1:) <$> mapM turnS [Coin, Push]
-- do notation version
-- hastyPersonS = do
--   a1 <- turnS Push
--   if a1 == Open
--   then
--     pure [a1]
--   else
--     (a1:) <$> mapM turnS [Coin, Push]

-- Implement tuesdayS using sequence or mapM
tuesdayS :: State TurnstileState [TurnstileOutput]
tuesdayS = sequence actions <&> concat
  where
    actions = [regularPersonS, hastyPersonS, distractedPersonS, hastyPersonS]

-- Implement saveCoinsS :: [TurnstileInput] -> State TurnstileState Int that potentially processes all of the given inputs,
-- but will skip a Coin input if the previous input generated a Thanks.
-- It returns the number of Coin inputs that were skipped.
-- E.g. evalState (saveCoinsS [Push, Coin, Coin, Coin, Push, Push, Coin, Push]) Locked should give 2.
saveCoinsS :: [TurnstileInput] -> State TurnstileState Int
saveCoinsS is =
  foldM f (Nothing, 0) is <&> snd
  where
    f ::
      (Maybe TurnstileOutput, Int)
      -> TurnstileInput
      -> State TurnstileState (Maybe TurnstileOutput, Int)
    f (Just Thank, cnt) Coin = pure (Just Thank, cnt+1)
    f (_         , cnt) i    = turnS i <&> Just <&> (, cnt)

-- Implement sequenceUntil :: (a -> Bool) -> [State s a] -> State s [a].
-- It processes each of the inputs until one of them generates a value that matches the predicate,
-- then processes no more.
-- E.g. evalState (sequenceUntil (== Open) [coinS, coinS, coinS, pushS, pushS, coinS]) Locked should give [Thank,Thank,Thank,Open]
sequenceUntil :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceUntil _ []     = pure []
sequenceUntil p (s:ss) =
  s >>= \a ->
    if p a
    then pure [a]
    else (a:) <$> sequenceUntil p ss 
