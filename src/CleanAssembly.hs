module CleanAssembly where

import qualified Assembly as A
import Data.Data (toConstr)

-- | True if the two commands are the same type of command.
sameConstructor :: A.Instruction Int -> A.Instruction Int -> Bool
sameConstructor x y = toConstr x == toConstr y

-- | True if the command is one that changes a setting for all events after it.
isSetting :: A.Instruction Int -> Bool
isSetting x = case x of
  A.Note      {} -> False
  A.DNote     {} -> False
  A.Rest      {} -> False
  A.PitchBend {} -> False
  _              -> True

-- | Removes status change commands which are identical to a given one,
-- up until finding one which changes the status to something different.
cleanEvent :: A.Instruction Int -> [A.Instruction Int] -> [A.Instruction Int]
cleanEvent _ []       = []
cleanEvent x ys@(y : yt) = if sameConstructor x y
  then if x == y
    then cleanEvent x yt
    else ys
  else y : cleanEvent x yt

-- | Removes redundant status change commands.
cleanAssembly :: [A.Instruction Int] -> [A.Instruction Int]
cleanAssembly [] = []
cleanAssembly (x : xs) = (x :) $ cleanAssembly $ if isSetting x
  then cleanEvent x xs
  else xs

-- | Finds settings that have the same value at the end of the intro,
-- the start of the loop, and the end of the loop. Such settings can be removed
-- from the start of the loop.
cleanBeginLoop :: ([A.Instruction Int], [A.Instruction Int]) -> ([A.Instruction Int], [A.Instruction Int])
cleanBeginLoop = let
  dummySettings =
    [ A.NoteType {}
    , A.DSpeed {}
    , A.Octave {}
    , A.Vibrato {}
    , A.Duty {}
    , A.StereoPanning {}
    , A.Tempo {}
    ]
  clean setting (b, l) = let
    fnb = filter (sameConstructor setting) b
    fnl = filter (sameConstructor setting) l
    in case (reverse fnb, fnl, reverse fnl) of
      (x : _, y : _, z : _) | x == y && x == z
        -> (b, cleanEvent x l)
      _ -> (b, l)
  in foldr (.) id $ map clean dummySettings
