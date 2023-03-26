module Dice where

import Control.Applicative
import Control.Monad

import Safe
import System.Random

import State

rollPair :: StdGen -> ((Int, Int), StdGen)
rollPair s0 =
  let (r1, s1) = randomR (1,6) s0
      (r2, s2) = randomR (1,6) s1
  in ((r1, r2), s2)

-- Implement rollSix :: StdGen -> ([Int], StdGen) using randomR (1,6)
-- that returns a list representing the result of six consecutive throws.
rollSix :: StdGen -> ([Int], StdGen)
rollSix s0 =
  let (r1, s1) = randomR (1,6) s0
      (r2, s2) = randomR (1,6) s1
      (r3, s3) = randomR (1,6) s2
      (r4, s4) = randomR (1,6) s3
      (r5, s5) = randomR (1,6) s4
      (r6, s6) = randomR (1,6) s5
   in ([r1, r2, r3, r4, r5, r6], s6)

-- Implement rollN :: Int -> StdGen -> ([Int], StdGen).
-- This is a bit tricky! But possible using iterate and take, for example.
rollN :: Int -> StdGen -> ([Int], StdGen)
rollN n s0 =
  let pairs = take n $ iterate (randomR (1,6) . snd) (randomR (1,6) s0)
      (ints, stdGens) = unzip pairs
      lastStdGen = lastDef s0 stdGens
   in (ints, lastStdGen)


-- We're about to define rollDieS :: State StdGen Int.
-- Why don't you have a go at it first, and contemplate what it is and how it could help.
rollDieS :: State StdGen Int
rollDieS = state $ randomR (1,6)

-- Implement rollSixS :: State StdGen [Int] with the same behaviour as rollSix.
-- Use do notation and rollDieS.
rollSixS :: State StdGen [Int]
rollSixS = do
  r1 <- rollDieS
  r2 <- rollDieS
  r3 <- rollDieS
  r4 <- rollDieS
  r5 <- rollDieS
  r6 <- rollDieS
  return [r1, r2, r3, r4, r5, r6]

-- Implement rollNS :: Int -> State StdGen [Int] using replicateM
rollNS :: Int -> State StdGen [Int]
rollNS n = replicateM n rollDieS

-- Implement luckyDoubleS :: State StdGen Int.
-- It does a first throw.
-- If it's a 6 it throws again and returns the total of the two throws,
-- else it just returns the first throw.
luckyDoubleS :: State StdGen Int
luckyDoubleS =
  rollDieS >>= \r1 ->
    if r1 == 6
    then (r1 +) <$> rollDieS
    else pure r1
-- do notation version
-- luckyDoubleS = do
--   r1 <- rollDieS
--   if r1 == 6
--     then (r1 +) <$> rollDieS
--     else pure r1

-- Rewrite rollPairS using (<$>) and (<*>), or liftA2
-- ORIGINAL:
-- rollPairS :: State StdGen (Int, Int)
-- rollPairS = do
--   r1 <- rollDieS
--   r2 <- rollDieS
--   return (r1, r2)

-- NEW:
rollPairS :: State StdGen (Int, Int)
rollPairS = (,) <$> rollDieS <*> rollDieS

-- Implement happyDoubleS :: State StdGen Int,
-- which throws two dice and returns the sum of the first and the second,
-- but doubles the total if the first is a six. Code it using do notation.
happyDoubleS :: State StdGen Int
happyDoubleS = do
  r1 <- rollDieS
  r2 <- rollDieS
  let total = r1 + r2
  if r1 == 6
    then pure $ total * 2
    else pure total

-- Rewrite happyDoubleS using (<$>) and (<*>), or liftA2.
happyDoubleS' :: State StdGen Int
happyDoubleS' = liftA2 happy rollDieS rollDieS
  where happy a b = if a == 6 then 2 * (a + b) else a + b

-- Write randomElt :: [a] -> State StdGen a,
-- using put and get to access the StdGen state.
-- It can assume the list is non empty,
-- and should return a random element from within it.
randomElt :: [a] -> State StdGen a
randomElt as = do
  s0 <- get
  let (idx, s1) = randomR (0, length as - 1) s0
  put s1
  return $ as !! idx

-- Rewrite randomElt without using put or get.
randomElt' :: [a] -> State StdGen a
randomElt' as =
  (as !!) <$> idxS
  where
    idxS = state $ randomR (0, length as - 1)
