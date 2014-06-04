{
{-# OPTIONS_GHC -w #-}
module Scan (scan, Token(..)) where

import qualified Assembly
}

%wrapper "basic"

tokens :-

[\ \t]+ ;
\; ([^\n]*) ;

[\n\r] { const Newline }
\, { const Comma }

[0-9]+ { Int . read }
[CDEFGAB][_\#] { Key . Assembly.readKey }
snare[1-9]      { Drum . Assembly.readDrum }
triangle[1-3]   { Drum . Assembly.readDrum }
cymbal[1-3]     { Drum . Assembly.readDrum }
mutedsnare[1-4] { Drum . Assembly.readDrum }

note { const Note }
dnote { const DNote }
rest { const Rest }
notetype { const NoteType }
dspeed { const DSpeed }
octave { const Octave }
vibrato { const Vibrato }
duty { const Duty }
stereopanning { const StereoPanning }
unknownmusic0xee { const UnknownMusic0xEE }
pitchbend { const PitchBend }
tempo { const Tempo }
loopchannel { const LoopChannel }
callchannel { const CallChannel }
endchannel { const EndChannel }
togglecall { const ToggleCall }

[A-Za-z0-9_]+ { Label }
\:\: { const GlobalLabel }

{

data Token
  = Newline
  | Comma
  | Int Int
  | Key Assembly.Key
  | Drum Assembly.Drum
  | Label String
  | GlobalLabel
  | Note
  | DNote
  | Rest
  | NoteType
  | DSpeed
  | Octave
  | Vibrato
  | Duty
  | StereoPanning
  | UnknownMusic0xEE
  | PitchBend
  | Tempo
  | LoopChannel
  | CallChannel
  | EndChannel
  | ToggleCall
  deriving (Eq, Ord, Show, Read)

scan :: String -> [Token]
scan = alexScanTokens

}
