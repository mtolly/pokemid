{- |
Cleans up the assembly code by removing redundant status change commands,
both within a single section and across the loop boundary.
-}
module Clean where

import Assembly
import Data.Data (toConstr)
import Control.Applicative ((<$))

-- | True if the two commands are the same type of command.
sameConstructor :: Instruction t -> Instruction t -> Bool
sameConstructor x y = toConstr (() <$ x) == toConstr (() <$ y)

-- | True if the command is one that changes a setting for all events after it.
isSetting :: Instruction t -> Bool
isSetting x = case x of
  Note      {} -> False
  DNote     {} -> False
  Rest      {} -> False
  _            -> True

-- | True if two instructions are equal, ignoring any tick-length parameters.
-- Intended to only be used for setting commands ('Vibrato', etc.).
nullEqual :: Instruction t -> Instruction t -> Bool
nullEqual x y = (() <$ x) == (() <$ y)

-- | Removes status change commands which are identical to a given one,
-- up until finding one which changes the status to something different.
cleanEvent :: Instruction t -> [Instruction t] -> [Instruction t]
cleanEvent _ []       = []
cleanEvent x ys@(y : yt) = if sameConstructor x y
  then if nullEqual x y
    then cleanEvent x yt
    else ys
  else y : cleanEvent x yt

-- | Removes redundant status change commands.
cleanAssembly :: [Instruction t] -> [Instruction t]
cleanAssembly [] = []
cleanAssembly (x : xs) = (x :) $ cleanAssembly $ if isSetting x
  then cleanEvent x xs
  else xs

-- | Finds settings that have the same value at the end of the intro,
-- the start of the loop, and the end of the loop. Such settings can be removed
-- from the start of the loop.
cleanLoop :: LoopForm t -> LoopForm t
cleanLoop = let
  dummySettings =
    [ NoteType {}
    , DSpeed {}
    , Octave {}
    , Vibrato {}
    , Duty {}
    , StereoPanning {}
    , Tempo {}
    ]
  clean _       (b, Nothing) = (b, Nothing)
  clean setting (b, Just l ) = let
    fnb = filter (sameConstructor setting) b
    fnl = filter (sameConstructor setting) l
    in case (reverse fnb, fnl, reverse fnl) of
      (x : _, y : _, z : _) | nullEqual x y && nullEqual x z
        -> (b, Just $ cleanEvent x l)
      _ -> (b, Just l)
  in foldr (.) id $ map clean dummySettings
