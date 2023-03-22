module TurnstileState1 where

import Data.Functor

import State
import Turnstile

coinS, pushS :: State TurnstileState TurnstileOutput
coinS = do
  put Unlocked
  pure Thank
-- coinS = put Unlocked >> pure Thank

pushS = do
  s <- get
  put Locked
  case s of
    Locked   -> pure Tut
    Unlocked -> pure Open
-- pushS =
--   get >>= \s ->
--     put Locked >>
--       case s of
--         Locked   -> pure Tut
--         Unlocked -> pure Open

mondayS :: State TurnstileState [TurnstileOutput]
mondayS = do
  a1 <- coinS
  a2 <- pushS
  a3 <- pushS
  a4 <- coinS
  a5 <- pushS
  return [a1, a2, a3, a4, a5]
-- shorter version
-- mondayS = sequence [coinS, pushS, pushS, coinS, pushS]


-- Implement the functions regularPersonS, distractedPersonS, hastyPersonS :: State TurnstileState [TurnstileOutput] using sequence.
-- You will need still need do notation for the third.
regularPersonS, distractedPersonS, hastyPersonS :: State TurnstileState [TurnstileOutput]

regularPersonS = sequence [coinS, pushS]

distractedPersonS = sequence [coinS]

hastyPersonS =
  pushS >>= \a1 ->
    if a1 == Open
    then
      pure [a1]
    else
      (a1:) <$> sequence [coinS, pushS]
-- do notation version
-- hastyPersonS = do
--   a1 <- pushS
--   if a1 == Open
--   then
--     pure [a1]
--   else
--     (a1:) <$> sequence [coinS, pushS]

-- Implement luckyPairS :: Bool -> State TurnstileState Bool
luckyPairS :: Bool -> State TurnstileState Bool
luckyPairS isDistracted
  | isDistracted = distractedPersonS >> pushS <&> (== Open)
  | otherwise    = regularPersonS >> pushS <&> (== Open)

-- Extend testTurnstile so that it also checks the state is set to Unlocked after a coin is inserted, regardless of the state beforehand.
-- And for good measure, have testTurnstile return the turnstile to it's original state when the testing is complete.
testTurnstile :: State TurnstileState Bool
testTurnstile = do
  s0 <- get

  -- test pushS
  put Locked
  check1 <- pushS
  put Unlocked
  check2 <- pushS

  -- test coinS
  put Locked
  _ <- coinS
  check3 <- get
  put Unlocked
  _ <- coinS
  check4 <- get

  -- reset original state
  put s0

  -- checks
  return (check1 == Tut && check2 == Open && check3 == Unlocked && check4 == Unlocked)
