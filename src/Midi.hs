module Midi where

-- midi
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
-- event-list
import qualified Data.EventList.Relative.TimeBody as RTB
-- non-negative
import qualified Numeric.NonNegative.Class as NNC
import qualified Numeric.NonNegative.Wrapper as NN
-- base
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

data Event
  = Off Int
  | Begin
  | End
  | Type Int Int
  | Vibrato Int Int Int
  | Duty Int
  | StereoPanning Int
  | PitchBend Int Int
  | Tempo Int Int
  | On Int
  deriving (Eq, Ord, Show, Read)

getEvent :: E.T -> Maybe Event
getEvent e = case e of
  E.MIDIEvent (C.Cons _ch (C.Voice (V.NoteOn p v))) ->
    Just $ (if V.fromVelocity v /= 0 then On else Off) $ V.fromPitch p
  E.MIDIEvent (C.Cons _ch (C.Voice (V.NoteOff p _v))) ->
    Just $ Off $ V.fromPitch p
  E.MetaEvent (M.TextEvent str) -> case words str of
    [] -> Nothing
    cmd : ints -> case (cmd, readMaybe $ "[" ++ concat ints ++ "]") of
      ("begin"        , Just []       ) -> Just Begin
      ("end"          , Just []       ) -> Just End
      ("vibrato"      , Just [x, y, z]) -> Just $ Vibrato x y z
      ("duty"         , Just [x]      ) -> Just $ Duty x
      ("notetype"     , Just [_, y, z]) -> Just $ Type y z
      ("notetype"     , Just [y, z]   ) -> Just $ Type y z
      ("stereopanning", Just [x]      ) -> Just $ StereoPanning x
      ("pitchbend"    , Just [x, y]   ) -> Just $ PitchBend x y
      _                                 -> Nothing
  E.MetaEvent (M.SetTempo t) ->
    Just $ Tempo `uncurry`
      quotRem (round $ (toRational t / 1000000) * 320) 256
  _ -> Nothing

fromEvent :: Event -> E.T
fromEvent e = case e of
  Off p -> voice0 $ V.NoteOff (V.toPitch p) (V.toVelocity 0)
  Begin -> E.MetaEvent $ M.TextEvent "begin"
  End -> E.MetaEvent $ M.TextEvent "end"
  Type x y -> textCmd "notetype" [x, y]
  Vibrato x y z -> textCmd "vibrato" [x, y, z]
  Duty x -> textCmd "duty" [x]
  StereoPanning x -> textCmd "stereopanning" [x]
  PitchBend x y -> textCmd "pitchbend" [x, y]
  Tempo x y -> E.MetaEvent $ M.SetTempo $ round $ (toRational (x * 256 + y) / 320) * 1000000
  On p -> voice0 $ V.NoteOn (V.toPitch p) (V.toVelocity 96)
  where voice0 = E.MIDIEvent . C.Cons (C.toChannel 0) . C.Voice
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

getTracks :: F.T -> [(String, RTB.T NN.Rational Event)]
getTracks (F.Cons F.Parallel (F.Ticks res) trks) = let
  ticksToRat = RTB.mapTime $ \t -> fromIntegral t / fromIntegral res
  name t = fromMaybe (error "getTracks: track without name") $ trackName t
  in case map ticksToRat trks of
    tempo : trk1 : trks' -> do
      (isFirst, t) <- (True, RTB.merge tempo trk1) : map ((,) False) trks'
      return (name $ if isFirst then trk1 else t, RTB.mapMaybe getEvent t)
    _ -> []
getTracks _ = error "getTracks: not a type-1 ticks-based MIDI"

fromTracks :: [(String, RTB.T NN.Rational Event)] -> F.T
fromTracks [] = F.Cons F.Parallel (F.Ticks 480) []
fromTracks ((name, trk) : trks) = let
  (tempos, rest) = RTB.partition isTempo trk
  isTempo (Tempo _ _) = True
  isTempo _           = False
  tempoTrack = fmap fromEvent tempos
  otherTracks = [ RTB.cons 0 (E.MetaEvent $ M.TrackName n) $ fmap fromEvent t | (n, t) <- (name, rest) : trks ]
  in F.Cons F.Parallel (F.Ticks 480)
    $ map (RTB.discretize . RTB.mapTime (* 480))
    $ tempoTrack : otherTracks
