{-# LANGUAGE BangPatterns #-}
module Error where

import MidiToAssembly (Simple(..))
import Midi (isChannelTrack)
import qualified Numeric.NonNegative.Wrapper as NN
import qualified Numeric.NonNegative.Class as NNC
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import Data.Maybe (mapMaybe)

getTempoTrack :: F.T -> RTB.T NN.Rational E.T
getTempoTrack (F.Cons F.Parallel (F.Ticks res) trks) = let
  ticksToRat = RTB.mapTime $ \t -> fromIntegral t / fromIntegral res
  in ticksToRat $ foldr RTB.merge RTB.empty $ filter (not . isChannelTrack) trks
getTempoTrack _ = error "getTempoTrack: not a proper MIDI file"

findEnd :: (NNC.C t) => RTB.T t (Simple t) -> t
findEnd rtb = case RTB.span (/= End) rtb of
  (before, after) -> case RTB.viewL after of
    Just ((dt, End), _) -> NNC.sum (RTB.getTimes before) `NNC.add` dt
    _ -> error "findEnd: couldn't find End event in event list"

isTimeSig :: E.T -> Maybe NN.Rational
isTimeSig (E.MetaEvent (M.TimeSig n d _ _)) = let
  denom = 2 ^^ (negate (fromIntegral d) :: Int)
  in Just $ 4 * fromIntegral n * denom
isTimeSig _ = Nothing

posnToMeasureBeats :: RTB.T NN.Rational E.T -> NN.Rational -> (Int, NN.Rational)
posnToMeasureBeats = go 4 0 where
  go tsig !msrs rtb !posn = let
    tsig' = case RTB.viewL $ RTB.collectCoincident rtb of
      Just ((0, now), _) -> case mapMaybe isTimeSig now of
        new : _ -> new
        _       -> tsig
      _ -> tsig
    in if tsig' > posn
      then (msrs, posn)
      else go tsig' (msrs + 1) (rtbDrop tsig' rtb) (posn - tsig')

rtbDrop :: NN.Rational -> RTB.T NN.Rational a -> RTB.T NN.Rational a
rtbDrop len rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> if len > dt
    then rtbDrop (len - dt) rtb'
    else RTB.cons (dt - len) x rtb'
