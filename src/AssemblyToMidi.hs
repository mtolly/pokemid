module AssemblyToMidi where

import Assembly
import Graph
import qualified Midi as M
-- base
import Control.Monad (guard)
-- event-list
import qualified Data.EventList.Relative.TimeBody as RTB
-- non-negative
import qualified Numeric.NonNegative.Wrapper as NN
-- containers
import qualified Data.Map as Map

loopFormToMidi :: Channel -> LoopForm Int -> RTB.T NN.Rational M.Event
loopFormToMidi ch (begin, loop) = let
  go inLoop speed octave insts = case insts of
    [] -> if inLoop
      then RTB.singleton 0 M.End
      else case loop of
        Nothing -> RTB.singleton 0 M.End
        Just l  -> RTB.cons 0 M.Begin $ go True speed octave l
    i : is -> case i of
      Note k tks -> let
        pitch = fromEnum k + 12 * case ch of
          Ch3 -> octave + 1
          _   -> octave + 2
        in RTB.cons 0 (M.On pitch) $ RTB.cons (ticksToLen tks) (M.Off pitch) $
          go inLoop speed octave is
      DNote tks d -> let
        pitch = fromEnum d
        in RTB.cons 0 (M.On pitch) $ RTB.cons (ticksToLen tks) (M.Off pitch) $
          go inLoop speed octave is
      Rest tks -> RTB.delay (ticksToLen tks) $ go inLoop speed octave is
      NoteType speed' vol fade ->
        RTB.cons 0 (M.NoteType vol fade) $ go inLoop speed' octave is
      DSpeed speed' -> go inLoop speed' octave is
      Octave octave' -> go inLoop speed octave' is
      Vibrato x y z -> RTB.cons 0 (M.Vibrato x y z) $ go inLoop speed octave is
      Duty x -> RTB.cons 0 (M.Duty x) $ go inLoop speed octave is
      StereoPanning x -> RTB.cons 0 (M.StereoPanning x) $ go inLoop speed octave is
      PitchBend x y -> RTB.cons 0 (M.PitchBend x y) $ go inLoop speed octave is
      Tempo x y -> RTB.cons 0 (M.Tempo x y) $ go inLoop speed octave is
      where ticksToLen tks = (fromIntegral tks / 4) * (fromIntegral speed / 12)
  in go False undefined undefined begin

channelTracks :: String -> Graph String Int -> [(String, RTB.T NN.Rational M.Event)]
channelTracks prefix g = do
  ch <- [Ch1 .. Ch4]
  let name = prefix ++ show ch
  guard $ Map.member name g
  let lform = loopForm name g
  return (name, loopFormToMidi ch lform)
