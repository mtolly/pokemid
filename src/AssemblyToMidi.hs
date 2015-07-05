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
  go inLoop speed vol octave insts = case insts of
    [] -> if inLoop
      then RTB.singleton 0 M.End
      else case loop of
        Nothing -> RTB.singleton 0 M.End
        Just l  -> RTB.cons 0 M.Begin $ go True speed vol octave l
    i : is -> case i of
      Note k tks pbend -> let
        addBend = case pbend of
          Just (x, y) -> RTB.cons 0 $ M.PitchBend x y
          Nothing     -> id
        pitch = fromEnum k + 12 * case ch of
          Ch3 -> octave + 1
          _   -> octave + 2
        in addBend
          $ RTB.cons 0 (M.On pitch midiVelocity)
          $ RTB.cons (ticksToLen tks) (M.Off pitch)
          $ go inLoop speed vol octave is
      DNote tks d -> let
        pitch = fromEnum d
        in RTB.cons 0 (M.On pitch 96)
          $ RTB.cons (ticksToLen tks) (M.Off pitch)
          $ go inLoop speed vol octave is
      Rest tks -> RTB.delay (ticksToLen tks) $ go inLoop speed vol octave is
      NoteType speed' vol' fade ->
        RTB.cons 0 (M.NoteType vol' fade) $ go inLoop speed' vol' octave is
      DSpeed speed' -> go inLoop speed' vol octave is
      Octave octave' -> go inLoop speed vol octave' is
      Vibrato x y z -> RTB.cons 0 (M.Vibrato x y z) $ go inLoop speed vol octave is
      Duty x -> RTB.cons 0 (M.Duty x) $ go inLoop speed vol octave is
      Volume l r -> RTB.cons 0 (M.Volume l r) $ go inLoop speed vol octave is
      StereoPanning x -> RTB.cons 0 (M.StereoPanning x) $ go inLoop speed vol octave is
      Tempo x -> RTB.cons 0 (M.Tempo x) $ go inLoop speed vol octave is
      TogglePerfectPitch -> RTB.cons 0 M.TogglePerfectPitch $ go inLoop speed vol octave is
      -- TODO
      ExecuteMusic -> go inLoop speed vol octave is
      DutyCycle _ -> go inLoop speed vol octave is
      where ticksToLen tks = (fromIntegral tks / 4) * (fromIntegral speed / 12)
            midiVelocity = 8 * (vol + 1) - 1
            -- This maps [0 .. 15] to [7, 15 .. 127].
  in go
    False -- is in loop
    (error $ "loopFormToMidi: note speed not defined in " ++ show ch) -- speed
    (error $ "loopFormToMidi: note volume not defined in " ++ show ch) -- volume
    3 -- octave (don't rely on this default)
    begin -- list of instructions to process

channelTracks :: String -> Graph String Int -> [(String, RTB.T NN.Rational M.Event)]
channelTracks prefix g = do
  ch <- [Ch1 .. Ch4]
  let name = prefix ++ show ch
  guard $ Map.member name g
  let lform = loopForm name g
  return (name, loopFormToMidi ch lform)
