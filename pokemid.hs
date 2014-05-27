{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
{- |

Pokémon Red/Blue MIDI importer

By Michael Tolly (<mailto:miketolly@gmail.com miketolly@gmail.com>)

Done:

  * Converts from MIDI durations to @notetype@/@dspeed@ and @(d)note@ commands.
  * Converts from MIDI pitches to octave and note commands, with appropriate
    octave offsets for Ch1/Ch2 and Ch3. For Ch4, associates pitches
    with the available percussion sounds.
  * Converts from MIDI tempos to tempo commands.
  * Supports most note modifiers for the Red/Blue music engine
    (@pitchbend@, @vibrato@, @duty@, @stereopanning@).
  * Error checking on events in the middle of notes.

Todo:

  * Condense repeated blocks of code using @loopchannel@ or @callchannel@.
  * Error checking for out-of-range numbers.
  * @unknownmusic0xee@, used only for fade-in at the start of the Rocket HQ theme.
  * Extend to @pokecrystal@?

-}
module Main where

-- midi
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as M
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
-- event-list
import qualified Data.EventList.Relative.TimeBody as RTB
-- non-negative
import qualified Numeric.NonNegative.Class as NNC
import qualified Numeric.NonNegative.Wrapper as NN
-- containers
import qualified Data.Set as Set
-- base
import Control.Applicative ((<$>))
import Control.Monad (forM_, guard)
import Data.Char (toLower)
import Data.Data (Data, Typeable, toConstr)
import Data.List (intercalate, isInfixOf)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, catMaybes, fromJust)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-- | These constructors are ordered very intentionally: first 'Off', then
-- 'Begin' and 'End', then 'Type' and 'Midi', then 'On'. This allows us to use
-- 'RTB.normalize' to read simultaneous events in the correct order.
data MidiEvent
  = Off Int
  | Begin
  | End
  | Type Int Int
  | Midi Event
  | On Int
  deriving (Eq, Ord, Show, Read, Data, Typeable)

-- | Constructor ordering doesn't matter because these go in a list anyway.
data AsmEvent
  = Note Key Int
  | DNote Int Drum
  | Rest Int
  | NoteType Int Int Int
  | DSpeed Int
  | Octave Int
  | Asm Event
  deriving (Eq, Ord, Show, Read, Data, Typeable)

data Event
  = Vibrato Int Int Int
  | Duty Int
  | StereoPanning Int
  | PitchBend Int Int
  | Tempo Int Int
  deriving (Eq, Ord, Show, Read, Data, Typeable)

getEvent :: E.T -> Maybe MidiEvent
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
      ("vibrato"      , Just [x, y, z]) -> Just $ Midi $ Vibrato x y z
      ("duty"         , Just [x]      ) -> Just $ Midi $ Duty x
      ("notetype"     , Just [_, y, z]) -> Just $ Type y z
      ("notetype"     , Just [y, z]   ) -> Just $ Type y z
      ("stereopanning", Just [x]      ) -> Just $ Midi $ StereoPanning x
      ("pitchbend"    , Just [x, y]   ) -> Just $ Midi $ PitchBend x y
      _                                 -> Nothing
  E.MetaEvent (M.SetTempo t) ->
    Just $ Midi $ Tempo `uncurry`
      quotRem (round $ (toRational t / 1000000) * 320) 256
  _ -> Nothing

trackName :: (NNC.C t) => RTB.T t E.T -> Maybe String
trackName rtb = case RTB.viewL $ RTB.collectCoincident rtb of
  Just ((dt, xs), _) | dt == NNC.zero ->
    case [ s | E.MetaEvent (M.TrackName s) <- xs ] of
      [n] -> Just n
      _   -> Nothing
  _ -> Nothing

getTracks :: F.T -> [(String, RTB.T NN.Rational MidiEvent)]
getTracks (F.Cons F.Parallel (F.Ticks res) trks) = let
  ticksToRat = RTB.mapTime $ \t -> fromIntegral t / fromIntegral res
  name t = fromMaybe (error "getTracks: track without name") $ trackName t
  in case map ticksToRat trks of
    tempo : trk1 : trks' -> do
      (isFirst, t) <- (True, RTB.merge tempo trk1) : map ((,) False) trks'
      return (name $ if isFirst then trk1 else t, RTB.mapMaybe getEvent t)
    _ -> []
getTracks _ = error "getTracks: not a type-1 ticks-based MIDI"

data Channel = Ch1 | Ch2 | Ch3 | Ch4
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

data Key = C_ | Cs | D_ | Ds | E_ | F_ | Fs | G_ | Gs | A_ | As | B_
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

data Drum = Snare Int | Triangle Int | Cymbal Int | MutedSnare Int
  deriving (Eq, Ord, Show, Read, Data, Typeable)

drumList :: [Drum]
drumList
  =  map Snare [1..5]
  ++ map Triangle [1, 2]
  ++ map Snare [6..9]
  ++ map Cymbal [1..3]
  ++ [MutedSnare 1, Triangle 3]
  ++ map MutedSnare [2..4]

readPitch :: Channel -> Int -> Either (Int, Key) Drum
readPitch Ch4 p = Right $ fromMaybe (Snare 1) $ lookup p $ zip [0..] drumList
readPitch Ch3 p = case quotRem p 12 of (q, r) -> Left (q - 1, toEnum r)
-- for Ch3, midi 72 is octave 5 note C_
readPitch _   p = case quotRem p 12 of (q, r) -> Left (q - 2, toEnum r)
-- for Ch1/Ch2, midi 72 is octave 4 note C_

data FullNote a = FullNote
  { vibrato       :: (Int, Int, Int)
  , duty          :: Int
  , stereoPanning :: Int
  , pitchBend     :: Maybe (Int, Int)
  , pitch         :: Either (Int, Key) Drum
  , noteType      :: (Int, Int)
  , noteLength    :: a
  } deriving (Eq, Ord, Show, Read, Functor)

defaultNote :: FullNote a
defaultNote = FullNote
  { vibrato = (0, 0, 0)
  , duty = 0
  , stereoPanning = 119
  , pitchBend = Nothing
  , pitch = undefined
  , noteType = (10, 0)
  , noteLength = undefined
  }

-- | Like 'MidiEvent', the constructors are ordered intentionally: first
-- 'SBegin' and 'SEnd', then 'STempo', then 'SNote'.
data Simple a
  = SBegin
  | SEnd
  | STempo Int Int
  | SNote (FullNote a)
  deriving (Eq, Ord, Show, Read, Functor)

findOff :: (NNC.C t) => Int -> RTB.T t MidiEvent -> Maybe t
findOff p rtb = let
  match (Off p') | p == p' = True
  match _                  = False
  in case RTB.span (not . match) rtb of
    (before, after) -> do
      ((dt, _), _) <- RTB.viewL after
      return $ NNC.sum (RTB.getTimes before) `NNC.add` dt

simplify :: (NNC.C t) => Channel -> RTB.T t MidiEvent -> RTB.T t (Simple t)
simplify ch = go defaultNote . RTB.normalize where
  go fn rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, x), rtb') -> case x of
      Off _ -> RTB.delay dt $ go fn rtb'
      Begin -> RTB.cons dt SBegin $ go fn rtb'
      Type a b -> RTB.delay dt $ go (fn { noteType = (a, b) }) rtb'
      Midi e -> case e of
        Vibrato a b c -> RTB.delay dt $ go (fn { vibrato = (a, b, c) }) rtb'
        Duty a -> RTB.delay dt $ go (fn { duty = a }) rtb'
        StereoPanning a -> RTB.delay dt $ go (fn { stereoPanning = a }) rtb'
        PitchBend a b -> RTB.delay dt $ go (fn { pitchBend = Just (a, b) }) rtb'
        Tempo a b -> RTB.cons dt (STempo a b) $ go fn rtb'
      On p -> case findOff p rtb' of
        Nothing -> error $ "simplify: no note off found for pitch " ++ show p
        Just len -> RTB.cons dt
          (SNote $ fn { pitch = readPitch ch p, noteLength = len })
          $ go (fn { pitchBend = Nothing }) rtb'
      End -> RTB.cons dt SEnd $ go fn rtb'

splitLoop :: (NNC.C t) =>
  RTB.T t (Simple t) -> (RTB.T t (Simple t), Maybe (RTB.T t (Simple t)))
splitLoop rtb = case RTB.span (`notElem` [SBegin, SEnd]) $ RTB.normalize rtb of
  (begin, loop) -> case RTB.viewL loop of
    Just ((dt, SBegin), rtb') -> (RTB.snoc begin dt SEnd, Just rtb')
    _                         -> (rtb                   , Nothing  )

-- | All possible lengths that can be encoded in the music engine format.
encodeLengths :: [(NN.Rational, (Int, Int))]
encodeLengths = do
  let firstSpds = [12,6,8,4]
  spd <- firstSpds ++ filter (`notElem` firstSpds) [1..15]
  tks <- [15,14..1]
  return ((fromIntegral tks / 4) * (fromIntegral spd / 12), (spd, tks))

-- | The set of all note lengths that can be represented.
encodeable :: Set.Set NN.Rational
encodeable = Set.fromList $ map fst encodeLengths

-- | Tries to combine encodeable lengths to a make a rest length.
encodeSum :: NN.Rational -> Maybe [(Int, Int)]
encodeSum 0   = Just []
encodeSum rat = case lookup rat encodeLengths of
  Just p  -> Just [p]
  Nothing -> case filter ((< rat) . fst) encodeLengths of
    [] -> Nothing
    ps -> listToMaybe $ mapMaybe f ps
    where f (r, enc) = fmap (enc :) $ encodeSum $ rat - r

encode :: Channel -> RTB.T NN.Rational (Simple NN.Rational) -> [AsmEvent]
encode ch = go 0 0 . RTB.normalize where
  -- like RTB.decreaseStart but errors instead of flooring at 0
  decreaseStart t rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, x), rtb') -> if t <= dt
      then RTB.cons (dt - t) x rtb'
      else error "encode: note overlaps other event"
  go ntx nty rtb = case RTB.viewL rtb of
    Nothing -> []
    Just ((dt, x), rtb') -> case x of
      SBegin -> error "encode: found loop beginning"
      SEnd -> rest
      STempo a b -> rest ++ [Asm $ Tempo a b] ++ go ntx nty rtb'
      SNote fn -> let
        newLength = fromMaybe
          (error $ "note length too short to encode: " ++ show (noteLength fn))
          (Set.lookupLE (noteLength fn) encodeable)
          -- lookupLE shortens the length if it cannot be represented exactly
        (spd, tks) = fromJust $ lookup newLength encodeLengths
        in rest ++ catMaybes
          [ Just $ Asm $ case vibrato fn of (a, b, c) -> Vibrato a b c
          , guard (ch `elem` [Ch1, Ch2]) >> Just (Asm $ Duty $ duty fn)
          , Just $ Asm $ StereoPanning $ stereoPanning fn
          , Just $ if ch == Ch4
            then DSpeed spd
            else uncurry (NoteType spd) $ noteType fn
          , case pitch fn of Left (oct, _) -> Just $ Octave oct; _ -> Nothing
          , Asm . uncurry PitchBend <$> pitchBend fn
          , Just $ case pitch fn of
            Left (_, k) -> Note k tks
            Right drum  -> DNote tks drum
          ] ++ uncurry go (noteType fn) (decreaseStart newLength rtb')
      where rest = case encodeSum dt of
              Nothing -> error $ "encode: couldn't make rest length " ++ show dt
              Just ps -> concatMap (\(spd, tks) -> [restSpeed spd, Rest tks]) ps
            restSpeed spd = if ch == Ch4
              then DSpeed   spd
              else NoteType spd ntx nty

-- | True if the two commands are the same type of command.
sameConstructor :: AsmEvent -> AsmEvent -> Bool
sameConstructor (Asm x) (Asm y) = toConstr x == toConstr y
sameConstructor x       y       = toConstr x == toConstr y

-- | True if the command is one that changes a setting for all events after it.
isSetting :: AsmEvent -> Bool
isSetting x = case x of
  NoteType _ _ _      -> True
  DSpeed _            -> True
  Octave   _          -> True
  Asm (PitchBend _ _) -> False
  Asm _               -> True
  _                   -> False

-- | Removes status change commands which are identical to a given one,
-- up until finding one which changes the status to something different.
cleanEvent :: AsmEvent -> [AsmEvent] -> [AsmEvent]
cleanEvent _ []       = []
cleanEvent x ys@(y : yt) = if sameConstructor x y
  then if x == y
    then cleanEvent x yt
    else ys
  else y : cleanEvent x yt

-- | Removes redundant status change commands.
cleanAssembly :: [AsmEvent] -> [AsmEvent]
cleanAssembly [] = []
cleanAssembly (x : xs) = (x :) $ cleanAssembly $ if isSetting x
  then cleanEvent x xs
  else xs

showKey :: Key -> String
showKey k = case show k of
  [ch, 's'] -> [ch, '#']
  showk     -> showk

showDrum :: Drum -> String
showDrum = map toLower . filter (/= ' ') . show

toAssembly :: AsmEvent -> String
toAssembly asm = case asm of
  Note k len        -> f "note" [showKey k, show len]
  DNote len d       -> f "dnote" [show len, showDrum d]
  Rest len          -> f "rest" [show len]
  NoteType x y z    -> f "notetype" $ map show [x, y, z]
  DSpeed x          -> f "dspeed" [show x]
  Octave x          -> f "octave" [show x]
  Asm evt -> case evt of
    Vibrato x y z   -> f "vibrato" $ map show [x, y, z]
    Duty x          -> f "duty" [show x]
    StereoPanning x -> f "stereopanning" [show x]
    Tempo x y       -> f "tempo" $ map show [x, y]
    PitchBend x y   -> f "pitchbend" $ map show [x, y]
  where f cmd args = cmd ++ " " ++ intercalate ", " args

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [mid] -> do
      trks <- getTracks <$> Load.fromFile mid
      forM_ trks $ \(name, trk) -> let
        chan = case [ c | c <- [minBound..maxBound], show c `isInfixOf` name] of
          []     -> Ch1
          ch : _ -> ch
        (begin, loop) = splitLoop $ simplify chan trk
        asmLines = map toAssembly . cleanAssembly . encode chan
        printAsm = mapM_ (putStrLn . ('\t' : )) . asmLines
        in do
          putStrLn $ name ++ "::"
          printAsm begin
          case loop of
            Nothing -> putStrLn "\tendchannel"
            Just l -> do
              putStrLn ""
              putStrLn $ name ++ "_loop::"
              printAsm l
              putStrLn $ "\tloopchannel 0, "++name++"_loop"
          putStrLn ""
    _ -> do
      prog <- getProgName
      hPutStrLn stderr $ "Usage: "++prog++" in.mid > out.asm"
      exitFailure
