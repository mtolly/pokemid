module EmitAssembly where

import Assembly
import Data.List.Split (splitOn)
import Data.List (nub, maximumBy)
import Data.Ord (comparing)
import Control.Monad (guard)
import Data.Maybe (listToMaybe, mapMaybe)

possibleSubs :: [AsmInstruction] -> [[AsmInstruction]]
possibleSubs asm = let
  isCall (Left (CallChannel {})) = True
  isCall _                       = False
  -- Returns all unique subroutines we should look at which start with sub.
  growSub :: [[AsmInstruction]] -> [AsmInstruction] -> [[AsmInstruction]]
  growSub []                     _   = error "possibleSubs: no code???"
  growSub [_]                    _   = error "possibleSubs: sub doesn't appear?"
  growSub [_, _]                 _   = [] -- sub only appears once
  growSub split@(chunk : chunks) sub = let
    isLong = sum (map asmSize sub) > asmSize (Left $ CallChannel {})
    -- consistentNext is (Just x) if all of chunks start with x
    consistentNext = mapM listToMaybe chunks >>= getUniform
    getUniform []       = Nothing
    getUniform (x : xs) = guard (all (== x) xs) >> Just x
    possibleNexts = filter (not . isCall) $ nub $ mapMaybe listToMaybe chunks
    applyNext x = let
      go []                              = []
      go [c]                             = [c]
      go (c1 : c2@(y : _) : cs) | x == y = c1 : go (c2 : cs)
      go (c1 : c2         : cs)          = go $ (c1 ++ c2) : cs
      in go split
    in case consistentNext of
      Just next -> growSub (chunk : map tail chunks) (sub ++ [next])
      -- ^ We don't care about sub because adding next is strictly better.
      Nothing -> (if isLong then (sub :) else id) $ do
        next <- possibleNexts
        growSub (applyNext next) (sub ++ [next])
  in filter (not . isCall) (nub asm) >>= \inst ->
    growSub (splitOn [inst] asm) [inst]

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
