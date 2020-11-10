{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
module Assembly where

import           Data.Char (toLower)
import           Data.Data (Data, Typeable)
import           Data.List (intercalate)
import           Text.Read (readMaybe)

data Channel
  = Ch1 -- ^ Pulse 1
  | Ch2 -- ^ Pulse 2
  | Ch3 -- ^ Wave
  | Ch4 -- ^ Noise
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Key = C_ | Cs | D_ | Ds | E_ | F_ | Fs | G_ | Gs | A_ | As | B_
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

data Drum
  = Snare1
  | Snare2
  | Snare3
  | Snare4
  | Snare5
  | Triangle1
  | Triangle2
  | Snare6
  | Snare7
  | Snare8
  | Snare9
  | Cymbal1
  | Cymbal2
  | Cymbal3
  | MutedSnare1
  | Triangle3
  | MutedSnare2
  | MutedSnare3
  | MutedSnare4
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Data, Typeable)

type PitchSlide = Maybe (Int, Int, Key)

data Instruction t
  = Note               Key t PitchSlide
  | DrumNote           Drum t
  | Rest               t
  | NoteType           Int Int Int
  | DrumSpeed          Int
  | Octave             Int
  | Vibrato            Int Int Int
  | Volume             Int Int
  | StereoPanning      Int Int
  | Tempo              Int
  | TogglePerfectPitch
  | ExecuteMusic
  | DutyCycle          Int
  | DutyCyclePattern   Int Int Int Int
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

-- | The basic form of music we support: a sequence of instructions that plays
-- once, followed by an optional sequence that loops infinitely.
type LoopForm t = ([Instruction t], Maybe [Instruction t])

data Control label
  = Label      Bool label -- ^ Bool is true if label is exported
  | LocalLabel label
  | SoundLoop  Int label
  | SoundCall  label
  | SoundRet
  deriving (Eq, Ord, Show, Read, Functor)

type AsmLine = Either (Control String) (Instruction Int)

printAsm :: AsmLine -> String
printAsm (Left c) = case c of
  Label     b l -> l ++ if b then "::" else ":"
  LocalLabel  l -> "." ++ l
  SoundLoop n l -> makeInstruction "sound_loop" [show n, l]
  SoundCall l   -> makeInstruction "sound_call" [l]
  SoundRet      -> makeInstruction "sound_ret" []
printAsm (Right i) = case i of
  Note  k t slide -> let
    pb = case slide of
      Just (x, y, k') -> makeInstruction "pitch_slide" [show x, show y, show k'] ++ "\n"
      Nothing     -> ""
    note = makeInstruction "note" [showKey k, show t]
    in pb ++ note
  DrumNote      t d   -> makeInstruction "drum_note" [show t, show $ fromEnum d + 1]
  Rest          t     -> makeInstruction "rest" [show t]
  NoteType      x y z -> makeInstruction "note_type" [show x, show y, show z]
  DrumSpeed     n     -> makeInstruction "drum_speed" [show n]
  Octave        n     -> makeInstruction "octave" [show n]
  Vibrato       x y z -> makeInstruction "vibrato" [show x, show y, show z]
  DutyCycle     n     -> makeInstruction "duty_cycle" [show n]
  DutyCyclePattern a b c d -> makeInstruction "duty_cycle_pattern" [show a, show b, show c, show d]
  Volume        x y   -> makeInstruction "volume" [show x, show y]
  StereoPanning x y   -> makeInstruction "stereo_panning" [show x, show y] -- maybe should use the binary format
  Tempo         n     -> makeInstruction "tempo" [show n]
  TogglePerfectPitch  -> makeInstruction "toggle_perfect_pitch" []
  ExecuteMusic        -> makeInstruction "execute_music" []

makeInstruction :: String -> [String] -> String
makeInstruction cmd args = "\t" ++ cmd ++ if null args
  then ""
  else " " ++ intercalate ", " args

showKey :: Key -> String
showKey k = case show k of
  [c, 's'] -> [c, '#']
  showk    -> showk

showDrum :: Drum -> String
showDrum = map toLower . show

readKey :: String -> Maybe Key
readKey [c, '#'] = readMaybe [c, 's']
readKey s        = readMaybe s

-- | The size in bytes of an assembled instruction.
asmSize :: AsmLine -> Int
asmSize (Left c) = case c of
  Label      {} -> 0
  LocalLabel {} -> 0
  SoundLoop  {} -> 4
  SoundCall  {} -> 3
  SoundRet   {} -> 1
asmSize (Right i) = case i of
  Note        _ _ slide -> 1 + maybe 0 (const 3) slide
  DrumNote           {} -> 2
  Rest               {} -> 1
  NoteType           {} -> 2
  DrumSpeed          {} -> 1
  Octave             {} -> 1
  Vibrato            {} -> 3
  Volume             {} -> 2
  StereoPanning      {} -> 2
  Tempo              {} -> 3
  TogglePerfectPitch {} -> 1
  ExecuteMusic       {} -> 1
  DutyCycle          {} -> 2
  DutyCyclePattern   {} -> 2
