module Emit where

import Assembly
import Data.List.Split (splitOn)
import Data.List (nub, maximumBy, intercalate)
import Data.Ord (comparing)
import Control.Monad (guard)
import Data.Maybe (listToMaybe, mapMaybe)

possibleSubs :: [AsmLine] -> [[AsmLine]]
possibleSubs asm = let
  isCall (Left (CallChannel {})) = True
  isCall _                       = False
  -- Returns all unique subroutines we should look at which start with sub.
  growSub :: [[AsmLine]] -> [AsmLine] -> [[AsmLine]]
  growSub []                     _   = error "possibleSubs: no code???"
  growSub [_]                    _   = error "possibleSubs: sub doesn't appear?"
  growSub [_, _]                 _   = [] -- sub only appears once
  growSub (chunk : chunks) sub = let
    isLong = sum (map asmSize sub) > asmSize (Left $ CallChannel {})
    -- consistentNext is (Just x) if all of chunks start with x
    consistentNext = do
      nextInsts <- mapM listToMaybe chunks
      inst <- getUniform nextInsts
      guard $ not $ isCall inst
      Just inst
    getUniform []       = Nothing
    getUniform (x : xs) = guard (all (== x) xs) >> Just x
    possibleNexts = filter (not . isCall) $ nub $ mapMaybe listToMaybe chunks
    applyNext x = splitOn (sub ++ [x]) asm
    -- TODO: efficient applyNext, using (chunk:chunks) but correctly handling
    -- adjacent appearances of sub
    in (if isLong then (sub :) else id) $ case consistentNext of
      Just next -> growSub (chunk : map tail chunks) (sub ++ [next])
      -- Do we need to add sub 2 lines above? Not sure if it's strictly worse
      Nothing -> do
        next <- possibleNexts
        growSub (applyNext next) (sub ++ [next])
  in filter (not . isCall) (nub asm) >>= \inst ->
    growSub (splitOn [inst] asm) [inst]

replaceSub :: [AsmLine] -> String -> [AsmLine] -> [AsmLine]
replaceSub sub name asm =
  intercalate [Left $ CallChannel name] $ splitOn sub asm

optimize :: String -> LoopForm Int -> [AsmLine]
optimize name (begin, loop) = let
  size = sum . map asmSize
  go :: Bool -> Int -> [AsmLine] -> [AsmLine] -> [AsmLine]
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
