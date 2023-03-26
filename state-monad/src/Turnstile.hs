module Turnstile where

data TurnstileState = Locked | Unlocked
  deriving (Eq, Show)

data TurnstileOutput = Thank | Open | Tut
  deriving (Eq, Show)

coin :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _ = (Thank, Unlocked)

push :: TurnstileState -> (TurnstileOutput, TurnstileState)
push Locked   = (Tut , Locked)
push Unlocked = (Open, Locked)

-- Implement functions regularPerson, distractedPerson, hastyPerson :: TurnstileState -> ([TurnstileOutput], TurnstileState).
-- Given a TurnstileState, each should return the outputs (and updated state) of a different type of person.
-- A regularPerson always inserts a coin then pushes the arm.
-- A distractedPerson always inserts a coin then wanders off without going through.
-- A hastyPerson first pushes the arm without having inserted a coin.
-- If it opens (for example, they're following a distractedPerson) they go through.
-- If not (for example they're following a regularPerson) they'll then insert a coin and push the arm to go through.

regularPerson, distractedPerson, hastyPerson  :: TurnstileState -> ([TurnstileOutput], TurnstileState)

regularPerson s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
   in ([a1, a2], s2)

distractedPerson s0 =
  let (a1, s1) = coin s0
   in ([a1], s1)

hastyPerson s0 =
  let (a1, s1) = push s0
      (a2, s2) = coin s1
      (a3, s3) = push s2
   in if a1 == Open
      then ([a1], s1)
      else ([a1, a2, a3], s3)

-- Example function
monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
  let (a1, s1) = coin s0
      (a2, s2) = push s1
      (a3, s3) = push s2
      (a4, s4) = coin s3
      (a5, s5) = push s4
  in ([a1, a2, a3, a4, a5], s5)

-- Use these functions to implement tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState),
-- returning the outputs from this sequence of visitors: regularPerson, hastyPerson, distractedPerson, hastyPerson.
tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
tuesday s0 =
  let (as1, s1) = regularPerson s0
      (as2, s2) = hastyPerson s1
      (as3, s3) = distractedPerson s2
      (as4, s4) = hastyPerson s3
   in (as1 ++ as2 ++ as3 ++ as4, s4)

-- Fold version
-- tuesday s0 =
--   foldl'
--     f
--     ([], s0)
--     [regularPerson, hastyPerson, distractedPerson, hastyPerson]
--   where
--     f (as, s) person =
--       let (as', s') = person s
--        in (as ++ as', s')

-- Implement luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState)
-- representing two people attempting to use the turnstile in succession.
-- The first is either a regularPerson or a distractedPerson (depending on the Bool argument).
-- The second person will simply push the arm without inserting a coin and give up if they don't get through.
-- The Bool result should indicate whether the second person made it through.
luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState)
luckyPair isDistracted s0 =
  let firstPerson = if isDistracted then distractedPerson else regularPerson
      (_,  s1) = firstPerson s0
      (a2, s2) = push s1
   in (a2 == Open, s2)
