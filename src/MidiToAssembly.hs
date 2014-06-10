{-# LANGUAGE DeriveFunctor #-}
module MidiToAssembly where

import qualified Midi as M
import qualified Assembly as A
-- event-list
import qualified Data.EventList.Relative.TimeBody as RTB
-- non-negative
import qualified Numeric.NonNegative.Class as NNC
import qualified Numeric.NonNegative.Wrapper as NN
-- containers
import qualified Data.Set as Set
-- base
import Control.Applicative ((<|>), (<$>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe, catMaybes, fromJust)
import Data.Ratio (denominator)

data FullNote a = FullNote
  { vibrato       :: (Int, Int, Int)
  , duty          :: Int
  , stereoPanning :: Maybe Int
  , unknownMusic0xEE :: Maybe Int
  , pitchBend     :: A.PitchBend
  , pitch         :: Either (Int, A.Key) A.Drum
  , noteType      :: (Int, Int)
  , noteLength    :: a
  } deriving (Eq, Ord, Show, Read, Functor)

defaultNote :: FullNote a
defaultNote = FullNote
  { vibrato = (0, 0, 0)
  , duty = 0
  , stereoPanning = Nothing
  , unknownMusic0xEE = Nothing
  , pitchBend = Nothing
  , pitch = undefined
  , noteType = (10, 0)
  , noteLength = undefined
  }

-- | Like 'M.Event', the constructors are ordered intentionally: first
-- 'Begin' and 'End', then 'Tempo', then 'Note'.
data Simple a
  = Begin
  | End
  | Tempo Int Int
  | Note (FullNote a)
  deriving (Eq, Ord, Show, Read, Functor)

readPitch :: A.Channel -> Int -> Either (Int, A.Key) A.Drum
readPitch A.Ch4 p = Right $ if p > fromEnum (maxBound :: A.Drum)
  then minBound
  else toEnum p
readPitch A.Ch3 p = case quotRem p 12 of (q, r) -> Left (q - 1, toEnum r)
-- for Ch3, midi 72 is octave 5 note C_
readPitch _   p = case quotRem p 12 of (q, r) -> Left (q - 2, toEnum r)
-- for Ch1/Ch2, midi 72 is octave 4 note C_

findOff :: (NNC.C t) => Int -> RTB.T t M.Event -> Maybe t
findOff p rtb = let
  match (M.Off p') | p == p' = True
  match _                    = False
  in case RTB.span (not . match) rtb of
    (before, after) -> do
      ((dt, _), _) <- RTB.viewL after
      return $ NNC.sum (RTB.getTimes before) `NNC.add` dt

simplify :: (NNC.C t) => A.Channel -> RTB.T t M.Event -> RTB.T t (Simple t)
simplify ch = go defaultNote . RTB.normalize where
  go fn rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, x), rtb') -> case x of
      M.Off _ -> RTB.delay dt $ go fn rtb'
      M.Begin -> RTB.cons dt Begin $ go fn rtb'
      M.NoteType a b -> RTB.delay dt $ go (fn { noteType = (a, b) }) rtb'
      M.Vibrato a b c -> RTB.delay dt $ go (fn { vibrato = (a, b, c) }) rtb'
      M.Duty a -> RTB.delay dt $ go (fn { duty = a }) rtb'
      M.StereoPanning a -> RTB.delay dt $ go (fn { stereoPanning = Just a }) rtb'
      M.UnknownMusic0xEE a -> RTB.delay dt $ go (fn { unknownMusic0xEE = Just a }) rtb'
      M.PitchBend a b -> RTB.delay dt $ go (fn { pitchBend = Just (a, b) }) rtb'
      M.Tempo a b -> RTB.cons dt (Tempo a b) $ go fn rtb'
      M.On p -> case findOff p rtb' of
        Nothing -> error $ "simplify: no note off found for pitch " ++ show p
        Just len -> RTB.cons dt
          (Note $ fn { pitch = readPitch ch p, noteLength = len })
          $ go (fn { pitchBend = Nothing }) rtb'
      M.End -> RTB.cons dt End $ go fn rtb'

splitLoop :: (NNC.C t) =>
  RTB.T t (Simple t) -> (RTB.T t (Simple t), Maybe (RTB.T t (Simple t)))
splitLoop rtb = case RTB.span (`notElem` [Begin, End]) $ RTB.normalize rtb of
  (begin, loop) -> case RTB.viewL loop of
    Just ((dt, Begin), rtb') -> (RTB.snoc begin dt End, Just rtb')
    _                        -> (rtb                  , Nothing  )

-- | All possible lengths that can be encoded in the music engine format.
encodeLengths :: [(NN.Rational, (Int, Int))]
encodeLengths = do
  let firstSpds = [12,6,8,4]
  spd <- firstSpds ++ filter (`notElem` firstSpds) [1..15]
  tks <- [16,15..1]
  return ((fromIntegral tks / 4) * (fromIntegral spd / 12), (spd, tks))

-- | The set of all note lengths that can be represented.
encodeable :: Set.Set NN.Rational
encodeable = Set.fromList $ map fst encodeLengths

encodeNote
  :: Int         -- ^ a preferred speed (speed of the last encoded note)
  -> NN.Rational -- ^ the note duration to encode
  -> Maybe (Int, Int)
encodeNote lastSpeed rat = let
  lengths = map snd $ filter ((rat ==) . fst) encodeLengths
  preferred = filter ((lastSpeed ==) . fst) lengths
  in listToMaybe preferred <|> listToMaybe lengths

-- | Tries to combine encodeable lengths to a make a rest length.
encodeSum
  :: Int         -- ^ a preferred speed (speed of the last encoded note)
  -> NN.Rational -- ^ the note duration to encode
  -> Maybe [(Int, Int)]
encodeSum _         0   = Just []
encodeSum lastSpeed rat = case encodeNote lastSpeed rat of
  Just p  -> Just [p]
  Nothing -> case 48 `rem` denominator (NN.toNumber rat) of
    0 -> case filter ((< rat) . fst) encodeLengths of
      [] -> Nothing
      ps -> listToMaybe $ mapMaybe f ps
      where f (r, enc) = fmap (enc :) $ encodeSum (fst enc) $ rat - r
    _ -> Nothing -- rest length is not multiple of 1/48 beat; unencodeable

encode ::
  A.Channel -> RTB.T NN.Rational (Simple NN.Rational) -> [A.Instruction Int]
encode ch = go 12 0 0 . RTB.normalize where
  -- like RTB.decreaseStart but errors instead of flooring at 0
  decreaseStart t rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, x), rtb') -> if t <= dt
      then RTB.cons (dt - t) x rtb'
      else error "encode: note overlaps other event"
  go ntSpeed ntVolume ntFade rtb = case RTB.viewL rtb of
    Nothing -> []
    Just ((dt, x), rtb') -> case x of
      Begin -> error "encode: found loop beginning"
      End -> rest
      Tempo a b -> rest ++ [A.Tempo a b] ++ go defaultSpeed ntVolume ntFade rtb'
      Note fn -> let
        newLength = fromMaybe
          (error $ "note length too short to encode: " ++ show (noteLength fn))
          (Set.lookupLE (noteLength fn) encodeable)
          -- lookupLE shortens the length if it cannot be represented exactly
        (spd, tks) = fromJust $ encodeNote defaultSpeed newLength
        in rest ++ catMaybes
          [ do
            guard $ ch /= A.Ch4
            Just $ case vibrato fn of (a, b, c) -> A.Vibrato a b c
          , do
            guard $ ch `elem` [A.Ch1, A.Ch2]
            Just $ A.Duty $ duty fn
          , A.StereoPanning <$> stereoPanning fn
          , A.UnknownMusic0xEE <$> unknownMusic0xEE fn
          , Just $ if ch == A.Ch4
            then A.DSpeed spd
            else uncurry (A.NoteType spd) $ noteType fn
          , either (Just . A.Octave . fst) (const Nothing) $ pitch fn
          , Just $ case pitch fn of
            Left (_, k) -> A.Note k tks $ pitchBend fn
            Right drum  -> A.DNote tks drum
          ] ++ uncurry (go spd) (noteType fn) (decreaseStart newLength rtb')
      where rest = case encodeSum ntSpeed dt of
              Nothing -> error $ "encode: couldn't make rest length " ++ show dt
              Just ps -> concatMap (\(spd, tks) -> [restSpeed spd, A.Rest tks]) ps
            restSpeed spd = if ch == A.Ch4
              then A.DSpeed   spd
              else A.NoteType spd ntVolume ntFade
            getSpeed (A.DSpeed   s    ) = Just s
            getSpeed (A.NoteType s _ _) = Just s
            getSpeed _                  = Nothing
            defaultSpeed = case mapMaybe getSpeed $ reverse rest of
              spd : _ -> spd
              []      -> ntSpeed
