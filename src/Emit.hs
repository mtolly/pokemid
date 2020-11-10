{- |
Takes assembly code as a 'LoopForm' and compiles it into labels and branches,
using subroutines to shrink repeated sections of code.
-}
module Emit where

import           Assembly
import           Control.Monad   (guard)
import           Data.List       (intercalate, maximumBy, nub)
import           Data.List.Split (splitOn)
import           Data.Maybe      (listToMaybe, mapMaybe)
import           Data.Ord        (comparing)

possibleSubs :: [AsmLine] -> [[AsmLine]]
possibleSubs asm = let
  isCall (Left (SoundCall {})) = True
  isCall _                     = False
  -- Returns all unique subroutines we should look at which start with sub.
  growSub :: [[AsmLine]] -> [AsmLine] -> [[AsmLine]]
  growSub []                     _   = error "possibleSubs: no code???"
  growSub [_]                    _   = error "possibleSubs: sub doesn't appear?"
  growSub [_, _]                 _   = [] -- sub only appears once
  growSub (chunk : chunks) sub = let
    isLong = sum (map asmSize sub) > asmSize (Left $ SoundCall "")
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
  intercalate [Left $ SoundCall name] $ splitOn sub asm

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
          subCode = [Left $ Label False subName] ++ sub ++ [Left SoundRet]
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
  optBegin = go False 0 [] $ [Left $ Label True name] ++ map Right begin ++ case loop of
    Nothing -> [Left SoundRet]
    Just _  -> []
  optLoop = case loop of
    Nothing -> []
    Just l  -> go True 0 [] $
      [Left $ Label False loopName] ++ map Right l ++ [Left $ SoundLoop 0 loopName]
  loopName = name ++ "_loop"
  in optBegin ++ optLoop
