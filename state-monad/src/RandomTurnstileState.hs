module RandomTurnstileState where

import System.Random

import State
import Turnstile
import TurnstileState2

getRandomS :: Random a => State StdGen a
getRandomS = state random

randomInputS :: State StdGen TurnstileInput
randomInputS = do
  b <- getRandomS
  return $ if b then Coin else Push

-- Implement randomTurnS, using get and put to access and set the combined state,
-- and runState to invoke randomInputS and turnS.
randomTurnS :: State (StdGen, TurnstileState) TurnstileOutput
randomTurnS = do
  (stdGen0, state0) <- get
  let (input, stdGen1) = runState randomInputS stdGen0
      (output, state1) = runState (turnS input) state0
  put (stdGen1, state1)
  pure output

processingFst :: State a o -> State (a,b) o
processingFst m = do
  (s1,s2) <- get
  let (o,s1') = runState m s1
  put (s1',s2)
  return o

-- Implement processingSnd.
processingSnd :: State b o -> State (a,b) o
processingSnd m = do
  (s1,s2) <- get
  let (o,s2') = runState m s2
  put (s1,s2')
  return o

-- Modify randomTurnS to use processingFst and processingSnd.
randomTurnS' :: State (StdGen, TurnstileState) TurnstileOutput
randomTurnS' =
  processingFst randomInputS >>= 
    processingSnd . turnS

data Lens cmb sub = Lens
  { view :: cmb -> sub,
    set  :: cmb -> sub -> cmb
  }

fstL :: Lens (a,b) a
fstL = Lens fst (\(_,y) x -> (x,y))

sndL :: Lens (a,b) b
sndL = Lens snd (\(x,_) y -> (x,y))

-- Implement processing that takes a Lens cmb sub parameter, and "converts" a State sub to a State cmb.
processing :: Lens cmb sub -> State sub a -> State cmb a
processing (Lens { view, set }) m = do
  cmb <- get
  let sub = view cmb
      (a, sub') = runState m sub
      cmb' = set cmb sub'
  put cmb'
  pure a

-- Rewrite randomTurnS using the processing function (and fstL and sndL).
randomTurnS'' :: State (StdGen, TurnstileState) TurnstileOutput
randomTurnS'' =
  processing fstL randomInputS >>=
    processing sndL . turnS
