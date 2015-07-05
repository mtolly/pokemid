{- |
Contains the simplified view of MIDI events that we care about, along with the
translation functions to/from \"raw\" MIDI events.
-}
module Midi where

import Assembly (Channel(..))
-- midi
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.General as G
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
-- event-list
import qualified Data.EventList.Relative.TimeBody as RTB
-- non-negative
import qualified Numeric.NonNegative.Class as NNC
import qualified Numeric.NonNegative.Wrapper as NN
-- base
import Data.Char (isSpace)
import Data.List (intercalate, isInfixOf, partition)
import Data.Maybe (fromMaybe)

{- |
A simplified view of the MIDI events we care about, ignoring things like MIDI
velocity and channels, and interpreting the special command text events.

The constructors are ordered intentionally: first 'Off', then 'Begin' and 'End',
then any pre-note settings, and finally 'On'. This lets us use 'RTB.normalize'
to process events in the correct order.
-}
data Event
  = Off Int
  | Begin
  | End
  | NoteType Int Int
  | Vibrato Int Int Int
  | Duty Int
  | Volume Int Int
  | StereoPanning Int
  | PitchBend Int Int
  | Tempo Int
  | TogglePerfectPitch
  | On Int Int -- ^ On pitch velocity
  deriving (Eq, Ord, Show, Read)

-- | Defined in recent versions of Text.Read,
-- but redefined here for compatibility.
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(x, s')] | all isSpace s' -> Just x
  _                          -> Nothing

getEvent :: E.T -> Maybe Event
getEvent e = case e of
  E.MIDIEvent (C.Cons _ch (C.Voice (V.NoteOn p v))) ->
    Just $ if V.fromVelocity v /= 0
      then On (V.fromPitch p) (V.fromVelocity v)
      else Off $ V.fromPitch p
  E.MIDIEvent (C.Cons _ch (C.Voice (V.NoteOff p _v))) ->
    Just $ Off $ V.fromPitch p
  E.MetaEvent (M.TextEvent str) -> case words str of
    [] -> Nothing
    cmd : ints -> case (cmd, readMaybe $ "[" ++ concat ints ++ "]") of
      ("begin"        , Just []       ) -> Just Begin
      ("end"          , Just []       ) -> Just End
      ("vibrato"      , Just [x, y, z]) -> Just $ Vibrato x y z
      ("duty"         , Just [x]      ) -> Just $ Duty x
      ("notetype"     , Just [_, y, z]) -> Just $ NoteType y z
      ("notetype"     , Just [y, z]   ) -> Just $ NoteType y z
      ("volume"       , Just [x, y]   ) -> Just $ Volume x y
      ("stereopanning", Just [x]      ) -> Just $ StereoPanning x
      ("pitchbend"    , Just [x, y]   ) -> Just $ PitchBend x y
      ("toggleperfectpitch", Just []  ) -> Just TogglePerfectPitch
      _                                 -> Nothing
  E.MetaEvent (M.SetTempo t) ->
    Just $ Tempo $ round $ (toRational t / 1000000) * 320
  _ -> Nothing

fromEvent :: C.Channel -> Event -> E.T
fromEvent midiChannel e = case e of
  Off p -> voice0 $ V.NoteOff (V.toPitch p) (V.toVelocity 0)
  Begin -> E.MetaEvent $ M.TextEvent "begin"
  End -> E.MetaEvent $ M.TextEvent "end"
  NoteType x y -> textCmd "notetype" [x, y]
  Vibrato x y z -> textCmd "vibrato" [x, y, z]
  Duty x -> textCmd "duty" [x]
  Volume x y -> textCmd "volume" [x, y]
  StereoPanning x -> textCmd "stereopanning" [x]
  PitchBend x y -> textCmd "pitchbend" [x, y]
  Tempo x -> E.MetaEvent $ M.SetTempo $ round $ (toRational x / 320) * 1000000
  TogglePerfectPitch -> E.MetaEvent $ M.TextEvent "toggleperfectpitch"
  On p v -> voice0 $ V.NoteOn (V.toPitch p) (V.toVelocity v)
  where voice0 = E.MIDIEvent . C.Cons midiChannel . C.Voice
        textCmd cmd args = E.MetaEvent $ M.TextEvent $ cmd ++ if null args
          then ""
          else " " ++ intercalate ", " (map show args)

trackName :: (NNC.C t) => RTB.T t E.T -> Maybe String
trackName rtb = case RTB.viewL $ RTB.collectCoincident rtb of
  Just ((dt, xs), _) | dt == NNC.zero ->
    case [ s | E.MetaEvent (M.TrackName s) <- xs ] of
      [n] -> Just n
      _   -> Nothing
  _ -> Nothing

isChannelTrack :: (NNC.C t) => RTB.T t E.T -> Bool
isChannelTrack trk = case trackName trk of
  Nothing -> False
  Just s  -> any (`isInfixOf` s) $ map show [Ch1 .. Ch4]

getTracks :: F.T -> [(String, RTB.T NN.Rational Event)]
getTracks (F.Cons F.Parallel (F.Ticks res) trks) = let
  ticksToRat = RTB.mapTime $ \t -> fromIntegral t / fromIntegral res
  name t = fromMaybe (error "getTracks: track without name") $ trackName t
  isCh1 str = drop (length str - 3) str == "Ch1"
  in case partition isChannelTrack $ map ticksToRat trks of
    (trks', tempos) -> case partition (isCh1 . name) trks' of
      ([ch1], notCh1) -> (name ch1, RTB.mapMaybe getEvent $ RTB.merge tempo ch1)
        : map (\t -> (name t, RTB.mapMaybe getEvent t)) notCh1
      _ -> error "getTracks: no Ch1 track to attach tempos to"
      where tempo = foldr RTB.merge RTB.empty tempos
getTracks _ = error "getTracks: not a type-1 ticks-based MIDI"

-- | Looks for a channel name in the track name.
getNamedChannel :: String -> Channel
getNamedChannel name = case [ c | c <- [Ch1 .. Ch4], show c `isInfixOf` name] of
  []     -> Ch1
  ch : _ -> ch

{- |
Converts a sequence of named tracks to a complete MIDI file.
The tempo events in the first track are extracted to form the MIDI tempo track.
The track names should have channel names (like \"Ch1\") somewhere inside them.
-}
fromTracks :: [(String, RTB.T NN.Rational Event)] -> F.T
fromTracks [] = F.Cons F.Parallel (F.Ticks 480) []
fromTracks ((name, trk) : trks) = let
  (tempos, rest) = RTB.partition isTempo trk
  isTempo (Tempo {}) = True
  isTempo _          = False
  tempoTrack = fmap (fromEvent $ C.toChannel 0) tempos
  otherTracks = do
    (n, t) <- (name, rest) : trks
    let channel = getNamedChannel n
        midiChannel = case channel of
          Ch1 -> G.instrumentChannels !! 0
          Ch2 -> G.instrumentChannels !! 1
          Ch3 -> G.instrumentChannels !! 2
          Ch4 -> G.drumChannel
        midiProgram = case channel of
          Ch1 -> G.instrumentToProgram G.Lead1Square
          Ch2 -> G.instrumentToProgram G.Lead1Square
          Ch3 -> G.instrumentToProgram G.Lead2Sawtooth
          Ch4 -> G.drumProgram
        pcEvent = E.MIDIEvent . C.Cons midiChannel . C.Voice . V.ProgramChange
    return
      $ RTB.cons 0 (E.MetaEvent $ M.TrackName n)
      $ RTB.cons 0 (pcEvent midiProgram)
      $ fmap (fromEvent midiChannel) t
  in F.Cons F.Parallel (F.Ticks 480)
    $ map (RTB.discretize . RTB.mapTime (* 480))
    $ tempoTrack : otherTracks
