module EmitAssembly where

import Assembly
import Data.List.Split (splitOn)
import Data.List (inits, tails, nub, maximumBy)
import Data.Ord (comparing)
import Control.Monad (guard)

possibleSubs :: [AsmInstruction] -> [[AsmInstruction]]
possibleSubs asm = let
  allSubs = nub $ inits asm >>= tails
  isLong sub = sum (map asmSize sub) > asmSize (Left $ CallChannel {})
  noCalls sub = null [ () | Left (CallChannel {}) <- sub ]
  in filter (\sub -> isLong sub && noCalls sub) allSubs

replaceSub :: [AsmInstruction] -> String -> [AsmInstruction] -> [AsmInstruction]
replaceSub sub name asm = let
  chunks = splitOn sub asm
  fcall = Left $ CallChannel name
  go []           = []
  go [chk]        = chk
  go (chk : chks) = case reverse chk of
    Right (PitchBend {}) : _ -> chk ++ sub ++ go chks
    -- ^ We can't put a function call here because it would split a pitchbend
    -- from its adjacent note.
    _                        -> chk ++ [fcall] ++ go chks
  in go chunks

optimize ::
  String -> ([Instruction Int], Maybe [Instruction Int]) -> [AsmInstruction]
optimize name (begin, loop) = let
  size = sum . map asmSize
  go :: Bool -> Int -> [AsmInstruction] -> [AsmInstruction] -> [AsmInstruction]
  go isLoop subNumber subsCode mainCode = let
    subName = if isLoop
      then name ++ "_loop_sub_" ++ show subNumber
      else name ++ "_sub_" ++ show subNumber
    possible = do
      sub <- possibleSubs mainCode
      let newMain = replaceSub sub subName mainCode
          subCode = [Left $ Label subName] ++ sub ++ [Left EndChannel]
          savedBytes = size mainCode - size newMain - size subCode
      guard $ savedBytes > 0
      return (savedBytes, (subsCode ++ subCode, newMain))
    in if null possible
      then if isLoop
        then mainCode ++ subsCode
        else subsCode ++ mainCode
      else let
        (bestSubs, bestMain) = snd $ maximumBy (comparing fst) possible
        in go isLoop (subNumber + 1) bestSubs bestMain
  optBegin = go False 0 [] $ [Left $ Label name] ++ map Right begin ++ case loop of
    Nothing -> [Left EndChannel]
    Just _  -> []
  optLoop = case loop of
    Nothing -> []
    Just l  -> go True 0 [] $
      [Left $ Label loopName] ++ map Right l ++ [Left $ LoopChannel 0 loopName]
  loopName = name ++ "_loop"
  in optBegin ++ optLoop
