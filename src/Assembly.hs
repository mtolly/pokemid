{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
module Assembly where

import Data.Char (toLower, toUpper)
import Data.Data (Data, Typeable)
import Data.List (intercalate)

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

type PitchBend = Maybe (Int, Int)

data Instruction t
  = Note          Key t PitchBend
  | DNote         t Drum
  | Rest          t
  | NoteType      Int Int Int
  | DSpeed        Int
  | Octave        Int
  | Vibrato       Int Int Int
  | Duty          Int
  | Volume        Int Int
  | StereoPanning Int
  | Tempo         Int Int
  deriving (Eq, Ord, Show, Read, Functor, Data, Typeable)

-- | The basic form of music we support: a sequence of instructions that plays
-- once, followed by an optional sequence that loops infinitely.
type LoopForm t = ([Instruction t], Maybe [Instruction t])

data Control label
  = Label       label
  | LoopChannel Int label
  | CallChannel label
  | EndChannel
  | ToggleCall
  deriving (Eq, Ord, Show, Read, Functor)

type AsmLine = Either (Control String) (Instruction Int)

printAsm :: AsmLine -> String
printAsm (Left c) = case c of
  Label       l   -> l ++ "::"
  LoopChannel n l -> makeInstruction "loopchannel" [show n, l]
  CallChannel l   -> makeInstruction "callchannel" [l]
  EndChannel      -> makeInstruction "endchannel" []
  ToggleCall      -> makeInstruction "togglecall" []
printAsm (Right i) = case i of
  Note  k t pbend -> let
    pb = case pbend of
      Just (x, y) -> makeInstruction "pitchbend" [show x, show y] ++ "\n"
      Nothing     -> ""
    note = makeInstruction "note" [showKey k, show t]
    in pb ++ note
  DNote t d       -> makeInstruction "dnote" [show t, showDrum d]
  _               -> case words $ show i of
    inst : ints   -> makeInstruction (map toLower inst) ints
    []            -> error "printAsm: shouldn't happen"

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

readKey :: String -> Key
readKey [c, '#'] = read [c, 's']
readKey s        = read s

readDrum :: String -> Drum
readDrum "" = error "readDrum: no parse"
readDrum ('m' : s) = read $ 'M' : map (\c -> if c == 's' then 'S' else c) s
readDrum (c   : s) = read $ toUpper c : s

-- | The size in bytes of an assembled instruction.
asmSize :: AsmLine -> Int
asmSize (Left c) = case c of
  Label         {} -> 0
  LoopChannel   {} -> 4
  CallChannel   {} -> 3
  EndChannel    {} -> 1
  ToggleCall    {} -> 1
asmSize (Right i) = case i of
  Note   _ _ pbend -> 1 + maybe 0 (const 3) pbend
  DNote         {} -> 2
  Rest          {} -> 1
  NoteType      {} -> 2
  DSpeed        {} -> 1
  Octave        {} -> 1
  Vibrato       {} -> 3
  Duty          {} -> 2
  Volume        {} -> 2
  StereoPanning {} -> 2
  Tempo         {} -> 3
