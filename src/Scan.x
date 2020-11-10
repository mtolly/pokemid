{
{-# OPTIONS_GHC -w #-}
module Scan (scan, Token(..), AlexPosn(..)) where

import qualified Assembly
import           Data.Bits  (bit)
import           Data.Maybe (fromJust)
}

%wrapper "posn"

tokens :-

[\ \t]+ ;
\; ([^\n]*) ;

[\n\r] { emit $ const Newline }
\, { emit $ const Comma }

\-? [0-9]+ { emit $ Int . read }
\% [01]+ { emit $ Int . readBinary . drop 1 }
[CDEFGAB][_\#] { emit $ Key . fromJust . Assembly.readKey }

rest { emit $ const Rest }
note_type { emit $ const NoteType }
note { emit $ const Note }
drum_note { emit $ const DrumNote }
drum_speed { emit $ const DrumSpeed }
octave { emit $ const Octave }
vibrato { emit $ const Vibrato }
volume { emit $ const Volume }
stereo_panning { emit $ const StereoPanning }
pitch_slide { emit $ const PitchSlide }
tempo { emit $ const Tempo }
sound_loop { emit $ const SoundLoop }
sound_call { emit $ const SoundCall }
sound_ret { emit $ const SoundRet }
toggle_perfect_pitch { emit $ const TogglePerfectPitch }
execute_music { emit $ const ExecuteMusic }
duty_cycle { emit $ const DutyCycle }
duty_cycle_pattern { emit $ const DutyCyclePattern }

[A-Za-z0-9_]+ { emit Label }
\: { emit $ const Colon }
\. { emit $ const Dot }

{

emit :: (String -> Token) -> AlexPosn -> String -> (AlexPosn, Token)
emit f pn s = (pn, f s)

data Token
  = Newline
  | Comma
  | Dot
  | Colon
  | Int Int
  | Key Assembly.Key
  | Label String
  | Note
  | DrumNote
  | Rest
  | NoteType
  | DrumSpeed
  | Octave
  | Vibrato
  | Volume
  | StereoPanning
  | PitchSlide
  | Tempo
  | SoundLoop
  | SoundCall
  | SoundRet
  | TogglePerfectPitch
  | ExecuteMusic
  | DutyCycle
  | DutyCyclePattern
  deriving (Eq, Ord, Show, Read)

scan :: String -> [(AlexPosn, Token)]
scan = alexScanTokens

-- takes a string of 0s and 1s, reads as unsigned int
readBinary :: String -> Int
readBinary = sum . map (\(i, b) -> bit i * read [b]) . zip [0..] . reverse

}
